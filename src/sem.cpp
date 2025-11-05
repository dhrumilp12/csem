#include <stdio.h>

extern "C" {
#include "cc.h"
#include "scan.h"
#include "sem.h"
#include "semutil.h"
#include "sym.h"
}

#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetMachine.h"
#include <cctype>
#include <cstdio>
#include <cstdlib>
#include <list>
#include <map>
#include <memory>
#include <string>
#include <utility>
#include <vector>

extern "C" {
void yyerror(const char* err) {
    fprintf(stderr, "Error: %s\n", err);
    exit(1);
}
}

#define MAXLOOPNEST 50
#define MAXLABELS 50
#define MAXGOTOS 50

using llvm::AllocaInst;
using llvm::ArrayRef;
using llvm::ArrayType;
using llvm::BasicBlock;
using llvm::BranchInst;
using llvm::Constant;
using llvm::ConstantAggregateZero;
using llvm::ConstantInt;
using llvm::Function;
using llvm::FunctionType;
using llvm::GlobalValue;
using llvm::GlobalVariable;
using llvm::Instruction;
using llvm::IntegerType;
using llvm::IRBuilder;
using llvm::LLVMContext;
using llvm::Module;
using llvm::outs;
using llvm::PointerType;
using llvm::Type;
using llvm::Value;
using std::map;
using std::string;
using std::vector;

extern int formalnum;                        /* number of formal arguments */
extern struct id_entry* formalvars[MAXLOCS]; /* entries for parameters */
extern int localnum;                         /* number of local variables  */
extern struct id_entry* localvars[MAXLOCS];  /* entries for local variables */

static LLVMContext TheContext;
static IRBuilder<> Builder(TheContext);
static std::unique_ptr<Module> TheModule;

template <typename T, typename... Args>
std::unique_ptr<T> make_unique(Args&&... args)
{
    return std::unique_ptr<T>(new T(std::forward<Args>(args)...));
}

static int label_index = 0;
int relexpr = 0;

struct loopscope {
    struct sem_rec* breaks;
    struct sem_rec* conts;
} lscopes[MAXLOOPNEST];

static int looplevel = 0;
struct loopscope* looptop = (struct loopscope*)NULL;

struct labelnode {
    const char* id; /* label string    */
    BasicBlock* bb; /* basic block for label */
} labels[MAXLABELS];

struct gotonode {
    const char* id;     /* label string in goto statement */
    BranchInst* branch; /* branch to temporary label */
} gotos[MAXGOTOS];      /* list of gotos to be backpatched */

int numgotos = 0;    /* number of gotos to be backpatched */
int numlabelids = 0; /* total label ids in function */

/*
 * startloopscope - start the scope for a loop
 */
void startloopscope()
{
    looptop = &lscopes[looplevel++];
    if (looptop > lscopes + MAXLOOPNEST) {
        fprintf(stderr, "loop nest too great\n");
        exit(1);
    }
    looptop->breaks = (struct sem_rec*)NULL;
    looptop->conts = (struct sem_rec*)NULL;
}

/*
 * endloopscope - end the scope for a loop
 */
void endloopscope()
{
    looplevel--;
    looptop--;
}

std::string new_label()
{
    return ("L" + std::to_string(label_index++));
}

BasicBlock*
create_tmp_label()
{
    return BasicBlock::Create(TheContext);
}

BasicBlock*
create_named_label(std::string label)
{
    Function* curr_func = Builder.GetInsertBlock()->getParent();
    BasicBlock* new_block = BasicBlock::Create(TheContext, label, curr_func);
    return new_block;
}

/*
 * convert an internal csem type (s_type or i_type) to an LLVM Type*
 */
Type* get_llvm_type(int type)
{
    switch (type & ~(T_ARRAY | T_ADDR)) {
        case T_INT:
            return Type::getInt32Ty(TheContext);
            break;
        case T_DOUBLE:
            return Type::getDoubleTy(TheContext);
            break;
        default:
            fprintf(stderr, "get_llvm_type: invalid type %x\n", type);
            exit(1);
            break;
    }
}

/*
 * HELPER UTILITY FUNCTIONS
 */
static Type* pointee_llvm_type(int t) {
    return get_llvm_type(t & ~(T_ARRAY | T_ADDR));
}

static bool isIntegerTy(int t) { return ((t & ~(T_ARRAY|T_ADDR)) == T_INT); }
static bool isDoubleTy (int t) { return ((t & ~(T_ARRAY|T_ADDR)) == T_DOUBLE); }

static struct sem_rec* as_rvalue(struct sem_rec* e) {
    // If it’s an lvalue (address), load it.
    if (e->s_type & T_ADDR) {
        Value* ptr = (Value*) e->s_value;
        Type*  ty  = pointee_llvm_type(e->s_type);
        Value* v   = Builder.CreateLoad(ty, ptr);
        return s_node((void*) v, e->s_type & ~T_ADDR);
    }
    return e;
}

// Cast helper: only int<->double for our language
static Value* cast_value(Value* v, int from_t, int to_t) {
    if (((from_t & ~(T_ARRAY|T_ADDR)) == (to_t & ~(T_ARRAY|T_ADDR)))) return v;
    if (isIntegerTy(from_t) && isDoubleTy(to_t))  return Builder.CreateSIToFP(v, get_llvm_type(T_DOUBLE));
    if (isDoubleTy(from_t)  && isIntegerTy(to_t)) return Builder.CreateFPToSI(v, get_llvm_type(T_INT));
    fprintf(stderr, "invalid cast\n"); exit(1);
}

// Unify numeric types for binary/relational ops
static void unify(Value*& L, int& Lt, Value*& R, int& Rt) {
    if (isDoubleTy(Lt) || isDoubleTy(Rt)) {
        if (!isDoubleTy(Lt)) L = cast_value(L, Lt, T_DOUBLE), Lt = T_DOUBLE;
        if (!isDoubleTy(Rt)) R = cast_value(R, Rt, T_DOUBLE), Rt = T_DOUBLE;
    } else {
        // keep both ints
    }
}

// Build a conditional-branch node with backpatch lists
static struct sem_rec* make_cond(Value* i1_cond) {
    BasicBlock* Tbb = create_tmp_label();
    BasicBlock* Fbb = create_tmp_label();
    BranchInst* br  = Builder.CreateCondBr(i1_cond, Tbb, Fbb);

    // We’ll return a sem_rec whose true/false lists each contain exactly one node:
    // s_value = the branch, s_bb = the successor placeholder we want to replace later.
    struct sem_rec* tnode = node((void*)br, (void*)Tbb, 0, NULL, NULL, NULL);
    struct sem_rec* fnode = node((void*)br, (void*)Fbb, 0, NULL, NULL, NULL);
    struct sem_rec* root  = node(NULL, NULL, 0, NULL, tnode, fnode);

    // We’re still in the old block; no terminator needed — br is the terminator.
    return root;
}

// Return a pair: (Value*, resulting-internal-type) for arithmetic ops +,-,*,/,%
static std::pair<Value*,int> do_arith_binop(const char* op, Value* L, int Lt, Value* R, int Rt) {
    // Promote to double if either side is double (C usual arithmetic conversions)
    // (We already have cast_value, unify helpers.)
    Value* LL = L; Value* RR = R; int LT = Lt; int RT = Rt;
    unify(LL, LT, RR, RT);

    if (isDoubleTy(LT)) {
        if (!strcmp(op,"+"))  return { Builder.CreateFAdd(LL, RR), T_DOUBLE };
        if (!strcmp(op,"-"))  return { Builder.CreateFSub(LL, RR), T_DOUBLE };
        if (!strcmp(op,"*"))  return { Builder.CreateFMul(LL, RR), T_DOUBLE };
        if (!strcmp(op,"/"))  return { Builder.CreateFDiv(LL, RR), T_DOUBLE };
        // C does not define % on floating types
        fprintf(stderr,"modulo on double\n"); exit(1);
    } else {
        if (!strcmp(op,"+"))  return { Builder.CreateAdd(LL, RR), T_INT };
        if (!strcmp(op,"-"))  return { Builder.CreateSub(LL, RR), T_INT };
        if (!strcmp(op,"*"))  return { Builder.CreateMul(LL, RR), T_INT };
        if (!strcmp(op,"/"))  return { Builder.CreateSDiv(LL, RR), T_INT };
        if (!strcmp(op,"%"))  return { Builder.CreateSRem(LL, RR), T_INT };
    }
    fprintf(stderr,"unknown arith op %s\n", op); exit(1);
}

// Bitwise/shift ops require integers in this language
static std::pair<Value*,int> do_bit_binop(const char* op, Value* L, int Lt, Value* R, int Rt) {
    // Force both to integers
    if (!isIntegerTy(Lt)) L = cast_value(L, Lt, T_INT), Lt = T_INT;
    if (!isIntegerTy(Rt)) R = cast_value(R, Rt, T_INT), Rt = T_INT;

    if (!strcmp(op,"|"))   return { Builder.CreateOr (L, R), T_INT };
    if (!strcmp(op,"^"))   return { Builder.CreateXor(L, R), T_INT };
    if (!strcmp(op,"&"))   return { Builder.CreateAnd(L, R), T_INT };
    if (!strcmp(op,"<<"))  return { Builder.CreateShl(L, R), T_INT };
    if (!strcmp(op,">>"))  return { Builder.CreateAShr(L, R), T_INT };

    fprintf(stderr,"unknown bitwise op %s\n", op); exit(1);
}


/*
 * backpatch - set temporary labels in the sem_rec to real labels
 *
 * LLVM API calls:
 *
 * llvm::dyn_cast<BranchInst>(Value*)
 * BranchInst::getNumSuccessors()
 * BranchInst::getSuccessor(unsigned)
 * BranchInst::setSuccessor(unsigned, BasicBlock*)
 */
void backpatch(struct sem_rec* p, void* bb_v) {
    if (!p) return;
    BasicBlock* real = (BasicBlock*) bb_v;
    for (struct sem_rec* q = p; q; q = q->s_link) {
        BranchInst* br = llvm::dyn_cast<BranchInst>((Value*) q->s_value);
        if (!br) continue;
        unsigned n = br->getNumSuccessors(); // 1 or 2
        for (unsigned i = 0; i < n; ++i) {
            if (br->getSuccessor(i) == (BasicBlock*) q->s_bb) {
                br->setSuccessor(i, real);
            }
        }
    }
}


/*
 * Global allocations. Globals are initialized to 0.
 */
void global_alloc(struct id_entry* p, int width) {
    string name(p->i_name);
    GlobalVariable* var;
    Type* type;
    Constant* init;

    if (p->i_type & T_ARRAY) {
        type = ArrayType::get(get_llvm_type(p->i_type), width);
        init = ConstantAggregateZero::get(type);
    } else {
        type = get_llvm_type(p->i_type);
        init = ConstantInt::get(get_llvm_type(T_INT), 0);
    }

    TheModule->getOrInsertGlobal(name, type);
    var = TheModule->getNamedGlobal(name);
    var->setInitializer(init);
    p->i_value = (void*)var;
}

/*
 * call - procedure invocation
 *
 * Grammar:
 * lval -> ID '(' ')'            { $$ = call($1, (struct sem_rec *) NULL); }
 * lval -> ID '(' exprs ')'      { $$ = call($1, $3); }
 *
 * LLVM API calls:
 * makeArrayRef(vector<Value*>)
 * IRBuilder::CreateCall(Function *, ArrayRef<Value*>)
 */
struct sem_rec* call(char* fname, struct sem_rec* args)
{
    struct id_entry* f = lookup(fname, 0);
    if (!f || !f->i_value) { fprintf(stderr,"call: unknown function %s\n", fname); exit(1); }

    Function* callee = llvm::dyn_cast<Function>((Value*)f->i_value);
    if (!callee) { fprintf(stderr,"call: symbol is not a function %s\n", fname); exit(1); }

    // Gather args in order from the exprs() forward list
    std::vector<Value*> argv;
    for (struct sem_rec* p = args; p; p = p->s_link) {
        p = as_rvalue(p);
        argv.push_back((Value*)p->s_value);
    }

    Value* ret = Builder.CreateCall(callee, ArrayRef<Value*>(argv));
    // If function returns void, we still return an int 0 node to keep things simple
    if (callee->getReturnType()->isVoidTy()) {
        return s_node((void*)ConstantInt::get(get_llvm_type(T_INT), 0), T_INT);
    }

    // Determine internal type from LLVM return type
    int rt = callee->getReturnType()->isDoubleTy() ? T_DOUBLE : T_INT;
    return s_node((void*)ret, rt);
}


/*
 * ccand - logical and
 *
 * Grammar:
 * cexpr -> cexpr AND m cexpr     { $$ = ccand($1, $3, $4); }
 *
 * LLVM API calls:
 * None
 */
struct sem_rec* ccand(struct sem_rec* e1, void* mlabel, struct sem_rec* e2) {
    // e1 true flows to mlabel where e2 is evaluated; e1 false falls through false
    backpatch(e1->s_true, mlabel);
    struct sem_rec* t = e2->s_true;
    struct sem_rec* f = merge(e1->s_false, e2->s_false);
    return node(NULL, NULL, 0, NULL, t, f);
}

/*
 * ccexpr - convert arithmetic expression to logical expression
 *
 * Grammar:
 * cexpr -> expr                  { $$ = ccexpr($1); }
 *
 * IRBuilder::CreateICmpNE(Value *, Value *)
 * IRBuilder::CreateFCmpONE(Value *, Value *)
 * IRBuilder::CreateCondBr(Value *, BasicBlock *, BasicBlock *)
 */
struct sem_rec* ccexpr(struct sem_rec* e) {
    e = as_rvalue(e);
    Value* v = (Value*) e->s_value;
    Value* cond = nullptr;

    if (isIntegerTy(e->s_type)) {
        cond = Builder.CreateICmpNE(v, ConstantInt::get(get_llvm_type(T_INT), 0));
    } else {
        cond = Builder.CreateFCmpONE(v, llvm::ConstantFP::get(get_llvm_type(T_DOUBLE), 0.0));
    }
    return make_cond(cond);
}


/*
 * ccnot - logical not
 *
 * Grammar:
 * cexpr -> NOT cexpr             { $$ = ccnot($2); }
 *
 * LLVM API calls:
 * None
 */
struct sem_rec* ccnot(struct sem_rec* e) {
    // swap true and false lists
    return node(NULL, NULL, 0, NULL, e->s_false, e->s_true);
}

/*
 * ccor - logical or
 *
 * Grammar:
 * cexpr -> cexpr OR m cexpr      { $$ = ccor($1, $3, $4); }
 *
 * LLVM API calls:
 * None
 */
struct sem_rec* ccor(struct sem_rec* e1, void* mlabel, struct sem_rec* e2) {
    // e1 false flows to mlabel where e2 is evaluated; e1 true falls through true
    backpatch(e1->s_false, mlabel);
    struct sem_rec* t = merge(e1->s_true, e2->s_true);
    struct sem_rec* f = e2->s_false;
    return node(NULL, NULL, 0, NULL, t, f);
}

/*
 * con - constant reference in an expression
 *
 * Grammar:
 * expr -> CON                   { $$ = con($1); }
 *
 * LLVM API calls:
 * ConstantInt::get(Type*, int)
 */
struct sem_rec* con(long long x)
{
    int length;
    struct id_entry *entry;
    char *xstr;

    length = snprintf(NULL, 0, "%lld", x);
    xstr = (char*)malloc(length+1);
    if (!xstr) { fprintf(stderr, "out of memory\n"); }
    snprintf(xstr, length+1, "%lld", x);

    if ((entry = lookup(xstr, 0)) == NULL) {
        entry = install (xstr, 0);
        entry->i_type = T_INT;
        entry->i_scope = GLOBAL;
        entry->i_defined = 1;
    }

    // Produce an LLVM i32 constant for this literal
    llvm::ConstantInt* c =
        llvm::ConstantInt::get(llvm::Type::getInt32Ty(TheContext),
                            (int32_t)x, /*isSigned*/ true);

    // Wrap as an rvalue semantic record with internal type T_INT
    return s_node((void*)c, T_INT);
}

/*
 * dobreak - break statement
 *
 * Grammar:
 * stmt -> BREAK ';'                { dobreak(); }
 *
 * LLVM API calls:
 * None
 */
void dobreak()
{
    fprintf(stderr, "sem: dobreak not implemented\n");
    return;
}

/*
 * docontinue - continue statement
 *
 * Grammar:
 * stmt -> CONTINUE ';'              { docontinue(); }
 *
 * LLVM API calls:
 * None
 */
void docontinue()
{
    fprintf(stderr, "sem: docontinue not implemented\n");
    return;
}

/*
 * dodo - do statement
 *
 * Grammar:
 * stmt -> DO m s lblstmt WHILE '(' m cexpr ')' ';' m
 *                { dodo($2, $7, $8, $11); }
 *
 * LLVM API calls:
 * None
 */
void dodo(void* m1, void* m2, struct sem_rec* cond, void* m3)
{
    fprintf(stderr, "sem: dodo not implemented\n");
    return;
}

/*
 * dofor - for statement
 *
 * Grammar:
 * stmt -> FOR '(' expro ';' m cexpro ';' m expro n ')' m s lblstmt n m
 *               { dofor($5, $6, $8, $10, $12, $15, $16); }
 *
 */
void dofor(void* m1, struct sem_rec* cond, void* m2, struct sem_rec* n1, void* m3,
           struct sem_rec* n2, void* m4)
{
    fprintf(stderr, "sem: dofor not implemented\n");
    return;
}

/*
 * dogoto - goto statement
 *
 * Grammar:
 * stmt -> GOTO ID ';'              { dogoto($2); }
 *
 * LLVM API calls:
 * IRBuilder::CreateBr(BasicBlock *)
 */
void dogoto(char* id)
{
    fprintf(stderr, "sem: dogoto not implemented\n");
    return;
}

/*
 * doif - one-arm if statement
 *
 * Grammar:
 * stmt -> IF '(' cexpr ')' m lblstmt m
 *         { doif($3, $5, $7); }
 *
 * LLVM API calls:
 * None
 */
void doif(struct sem_rec* cond, void* m1, void* m2) {
    backpatch(cond->s_true,  m1);  // then
    backpatch(cond->s_false, m2);  // fall-through (after)
}


/*
 * doifelse - if then else statement
 *
 * Grammar:
 * stmt -> IF '(' cexpr ')' m lblstmt ELSE n m lblstmt m
 *                { doifelse($3, $5, $8, $9, $11); }
 *
 * LLVM API calls:
 * None
 */
void doifelse(struct sem_rec* cond, void* m1, struct sem_rec* n,
              void* m2, void* m3)
{
    fprintf(stderr, "sem: doifelse not implemented\n");
    return;
}

/*
 * doret - return statement
 *
 * Grammar:
 * stmt -> RETURN ';'            { doret((struct sem_rec *) NULL); }
 * stmt -> RETURN expr ';'       { doret($2); }
 *
 * LLVM API calls:
 * IRBuilder::CreateRetVoid();
 * IRBuilder::CreateRet(Value *);
 */
void doret(struct sem_rec* e) {
    Function* F = Builder.GetInsertBlock()->getParent();
    Type*    RT = F->getReturnType();

    if (!e) {
        if (!RT->isVoidTy())
            Builder.CreateRet(ConstantInt::get(get_llvm_type(T_INT), 0));
        else
            Builder.CreateRetVoid();
        return;
    }

    e = as_rvalue(e);
    Value* v = (Value*) e->s_value;

    if (RT->isIntegerTy(32) && !isIntegerTy(e->s_type)) {
        v = cast_value(v, e->s_type, T_INT);
    } else if (RT->isDoubleTy() && !isDoubleTy(e->s_type)) {
        v = cast_value(v, e->s_type, T_DOUBLE);
    }
    Builder.CreateRet(v);
}


/*
 * dowhile - while statement
 *
 * Grammar:
 * stmt -> WHILE '(' m cexpr ')' m s lblstmt n m
 *                { dowhile($3, $4, $6, $9, $10); }
 *
 * LLVM API calls:
 * None
 */
void dowhile(void* m1, struct sem_rec* cond, void* m2,
             struct sem_rec* n, void* m3)
{
    fprintf(stderr, "sem: dowhile not implemented\n");
    return;
}

/*
 * exprs - form a list of expressions
 *
 * Grammar:
 * exprs -> exprs ',' expr        { $$ = exprs($1, $3); }
 *
 * LLVM API calls:
 * None
 */
struct sem_rec* exprs(struct sem_rec* l, struct sem_rec* e)
{
    // Append e at the end of singly-linked list l using s_link
    if (!l) return e;
    struct sem_rec* p = l;
    while (p->s_link) p = p->s_link;
    p->s_link = e;
    return l;
}


/*
 * fhead - beginning of function body
 *
 * Grammar:
 * fhead -> fname fargs '{' dcls  { fhead($1); }
 */
void fhead(struct id_entry* p)
{
    Type *func_type, *var_type;
    Value* arr_size;
    vector<Type*> func_args;
    GlobalValue::LinkageTypes linkage;
    FunctionType* FT;
    Function* F;
    BasicBlock* B;
    int i;
    struct id_entry* v;

    /* get function return type */
    func_type = get_llvm_type(p->i_type);

    /* get function argument types */
    for (i = 0; i < formalnum; i++) {
        func_args.push_back(get_llvm_type(formalvars[i]->i_type));
    }

    FT = FunctionType::get(func_type, ArrayRef(func_args), false);

    /* linkage is external if function is main */
    linkage = (strcmp(p->i_name, "main") == 0) ? Function::ExternalLinkage : Function::InternalLinkage;

    F = Function::Create(FT, linkage, p->i_name, TheModule.get());
    p->i_value = (void*)F;

    B = BasicBlock::Create(TheContext, "", F);
    Builder.SetInsertPoint(B);

    /*
     * Allocate instances of each parameter and local so they can be referenced
     * and mutated.
     */
    i = 0;
    for (auto& arg : F->args()) {

        v = formalvars[i++];
        arg.setName(v->i_name);
        var_type = get_llvm_type(v->i_type);
        arr_size = (v->i_width > 1) ? (ConstantInt::get(get_llvm_type(T_INT), v->i_width)) : NULL;

        v->i_value = Builder.CreateAlloca(var_type, arr_size, arg.getName());
        Builder.CreateStore(&arg, (Value*)v->i_value);
    }

    /* Create the instance of stack memory for each local variable */
    for (i = 0; i < localnum; i++) {
        v = localvars[i];
        var_type = get_llvm_type(v->i_type);
        arr_size = (v->i_width > 1) ? (ConstantInt::get(get_llvm_type(T_INT), v->i_width)) : NULL;

        v->i_value = Builder.CreateAlloca(var_type, arr_size, std::string(v->i_name));
    }
}

/*
 * fname - function declaration
 *
 * Grammar:
 * fname -> type ID               { $$ = fname($1, $2); }
 * fname -> ID                    { $$ = fname(T_INT, $1); }
 */
struct id_entry* fname(int t, char* id)
{
    struct id_entry* entry = lookup(id, 0);

    // add function to hash table if it doesn't exist
    if (!entry) {
        entry = install(id, 0);
    }

    // cannot have two functions of the same name
    if (entry->i_defined) {
        yyerror("cannot declare function more than once");
    }

    entry->i_type = t;
    entry->i_scope = GLOBAL;
    entry->i_defined = true;

    // need to enter the block to let hash table do internal work
    enterblock();
    // then need to reset argument count variables

    formalnum = 0;
    localnum = 0;

    return entry;
}

/*
 * ftail - end of function body
 *
 * Grammar:
 * func -> fhead stmts '}'       { ftail(); }
 */
void ftail()
{
    numgotos = 0;
    numlabelids = 0;
    leaveblock();
}

/*
 * id - variable reference
 *
 * Grammar:
 * lval -> ID                    { $$ = id($1); }
 * lval -> ID '[' expr ']'       { $$ = indx(id($1), $3); }
 *
 * LLVM API calls:
 * None
 */
struct sem_rec* id(char* x) {
    struct id_entry* e = lookup(x, 0);
    if (!e) {
        yyerror("undeclared identifier");
        e = install(x, -1);
        e->i_type = T_INT;
        e->i_scope = LOCAL;
        e->i_defined = 1;
    }
    // e->i_value is Alloca* (locals/params) or GlobalVariable* (globals)
    return s_node((void*) e->i_value, e->i_type | T_ADDR);
}


/*
 * indx - subscript
 *
 * Grammar:
 * lval -> ID '[' expr ']'       { $$ = indx(id($1), $3); }
 *
 * LLVM API calls:
 * makeArrayRef(vector<Value*>)
 * IRBuilder::CreateGEP(Type, Value *, ArrayRef<Value*>)
 */
struct sem_rec* indx(struct sem_rec* x, struct sem_rec* i)
{
    if (!(x->s_type & T_ADDR)) { fprintf(stderr,"index: base not address\n"); exit(1); }

    // ensure integer index
    i = as_rvalue(i);
    Value* idx = (Value*) i->s_value;
    if (!isIntegerTy(i->s_type))
        idx = cast_value(idx, i->s_type, T_INT);

    Value* base = (Value*) x->s_value;
    Value* gep  = nullptr;

    // Case 1: alloca
    if (auto *alloca = llvm::dyn_cast<AllocaInst>(base)) {
        Type* allocTy = alloca->getAllocatedType();
        if (allocTy->isArrayTy()) {
            Value* zero = ConstantInt::get(get_llvm_type(T_INT), 0);
            gep = Builder.CreateGEP(allocTy, base, ArrayRef<Value*>{zero, idx});
        } else {
            gep = Builder.CreateGEP(allocTy, base, ArrayRef<Value*>{idx});
        }
    }
    // Case 2: global
    else if (auto *gv = llvm::dyn_cast<GlobalVariable>(base)) {
        Type* gvTy = gv->getValueType(); // the pointee type of the global
        if (gvTy->isArrayTy()) {
            Value* zero = ConstantInt::get(get_llvm_type(T_INT), 0);
            gep = Builder.CreateGEP(gvTy, base, ArrayRef<Value*>{zero, idx});
        } else {
            gep = Builder.CreateGEP(gvTy, base, ArrayRef<Value*>{idx});
        }
    }
    // Case 3: generic pointer (e.g., param). Use semantic type to provide element type.
    else {
        Type* elemTy = pointee_llvm_type(x->s_type); // from your internal type system
        // Treat as pointer-to-element and step by idx
        gep = Builder.CreateGEP(elemTy, base, ArrayRef<Value*>{idx});
    }

    int elem_t = (x->s_type & ~(T_ADDR|T_ARRAY)); // base scalar type
    return s_node((void*)gep, elem_t | T_ADDR);
}

/*
 * labeldcl - process a label declaration
 *
 * Grammar:
 * labels -> ID ':'                { labeldcl($1); }
 * labels -> labels ID ':'         { labeldcl($2); }
 *
 * NOTE: All blocks in LLVM must have a terminating instruction (i.e., branch
 * or return statement -- fall-throughs are not allowed). This code must
 * ensure that each block ends with a terminating instruction.
 *
 * LLVM API calls:
 * IRBuilder::GetInsertBlock()
 * BasicBlock::getTerminator()
 * IRBuilder::CreateBr(BasicBlock*)
 * IRBuilder::SetInsertPoint(BasicBlock*)
 * BranchInst::setSuccessor(unsigned, BasicBlock*)
 */
void labeldcl(const char* id)
{
    fprintf(stderr, "sem: labeldcl not implemented\n");
    return;
}

/*
 * m - generate label and return next temporary number
 *
 * NOTE: All blocks in LLVM must have a terminating instruction (i.e., branch
 * or return statement -- fall-throughs are not allowed). This code must
 * ensure that each block ends with a terminating instruction.
 *
 * LLVM API calls:
 * IRBuilder::GetInsertBlock()
 * BasicBlock::getTerminator()
 * IRBuilder::CreateBr(BasicBlock*)
 * IRBuilder::SetInsertPoint(BasicBlock*)
 */
void* m() {
    BasicBlock* curr = Builder.GetInsertBlock();
    // If current block has no terminator, close it with a branch to a new block.
    if (!curr->getTerminator()) {
        BasicBlock* next = create_named_label(new_label());
        Builder.CreateBr(next);
        Builder.SetInsertPoint(next);
        return (void*) next;
    }
    // Already terminated (e.g., due to a condbr); just create a new insertion block.
    BasicBlock* next = create_named_label(new_label());
    Builder.SetInsertPoint(next);
    return (void*) next;
}


/*
 * n - generate goto and return backpatch pointer
 *
 * LLVM API calls:
 * IRBuilder::CreateBr(BasicBlock *)
 */
struct sem_rec* n() {
    BasicBlock* tgt = create_tmp_label();
    BranchInst* br  = Builder.CreateBr(tgt);
    // return a single-element list with br tagged by tgt
    return node((void*)br, (void*)tgt, 0, NULL, NULL, NULL);
}


/*
 * op1 - unary operators
 *
 * LLVM API calls:
 * IRBuilder::CreateLoad(Type, Value *)
 * IRBuilder::CreateNot(Value *)
 * IRBuilder::CreateNeg(Value *)
 * IRBuilder::CreateFNeg(Value *)
 */
struct sem_rec* op1(const char* op, struct sem_rec* y) {
    if (strcmp(op, "@") == 0) {
        // load from lvalue
        if (!(y->s_type & T_ADDR)) { fprintf(stderr, "load requires lvalue\n"); exit(1); }
        Type* ty = pointee_llvm_type(y->s_type);
        Value* v  = Builder.CreateLoad(ty, (Value*) y->s_value);
        return s_node((void*) v, y->s_type & ~T_ADDR);
    }

    // ensure rvalue
    y = as_rvalue(y);
    if (strcmp(op, "-") == 0) {
        if (isIntegerTy(y->s_type))  return s_node((void*)Builder.CreateNeg((Value*)y->s_value), T_INT);
        else                         return s_node((void*)Builder.CreateFNeg((Value*)y->s_value), T_DOUBLE);
    }
    if (strcmp(op, "~") == 0) {
        if (!isIntegerTy(y->s_type)) { fprintf(stderr, "bitwise ~ on non-int\n"); exit(1); }
        return s_node((void*)Builder.CreateNot((Value*)y->s_value), T_INT);
    }

    fprintf(stderr, "unhandled op1 %s\n", op); exit(1);
}


/*
 * op2 - arithmetic operators
 *
 * No LLVM API calls, but most functionality is abstracted to a separate
 * method used by op2, opb, and assign.
 *
 * The separate method uses the following API calls:
 * IRBuilder::CreateAdd(Value *, Value *)
 * IRBuilder::CreateFAdd(Value *, Value *)
 * IRBuilder::CreateSub(Value *, Value *)
 * IRBuilder::CreateFSub(Value *, Value *)
 * IRBuilder::CreateMul(Value *, Value *)
 * IRBuilder::CreateFMul(Value *, Value *)
 * IRBuilder::CreateSDiv(Value *, Value *)
 * IRBuilder::CreateFDiv(Value *, Value *)
 * IRBuilder::CreateSRem(Value *, Value *)
 * IRBuilder::CreateAnd(Value *, Value *)
 * IRBuilder::CreateOr(Value *, Value *)
 * IRBuilder::CreateXOr(Value *, Value *)
 * IRBuilder::CreateShl(Value *, Value *)
 * IRBuilder::CreateAShr(Value *, Value *)
 */
struct sem_rec* op2(const char* op, struct sem_rec* x, struct sem_rec* y)
{
    x = as_rvalue(x);
    y = as_rvalue(y);
    auto [val, rty] = do_arith_binop(op, (Value*)x->s_value, x->s_type,
                                         (Value*)y->s_value, y->s_type);
    return s_node((void*)val, rty);
}

/*
 * opb - bitwise operators
 *
 * No LLVM API calls, but most functionality is abstracted to a separate
 * method used by op2, opb, and assign. The comment above op2 lists the LLVM
 * API calls for this method.
 */
struct sem_rec* opb(const char* op, struct sem_rec* x, struct sem_rec* y)
{
    x = as_rvalue(x);
    y = as_rvalue(y);
    auto [val, rty] = do_bit_binop(op, (Value*)x->s_value, x->s_type,
                                       (Value*)y->s_value, y->s_type);
    return s_node((void*)val, rty);
}

/*
 * rel - relational operators
 *
 * Grammar:
 * cexpr -> expr EQ expr          { $$ = rel((char*) "==", $1, $3); }
 * cexpr -> expr NE expr          { $$ = rel((char*) "!=", $1, $3); }
 * cexpr -> expr LE expr          { $$ = rel((char*) "<=", $1, $3); }
 * cexpr -> expr GE expr          { $$ = rel((char*) ">=", $1, $3); }
 * cexpr -> expr LT expr          { $$ = rel((char*) "<",  $1, $3); }
 * cexpr -> expr GT expr          { $$ = rel((char*) ">",  $1, $3); }
 *
 * LLVM API calls:
 * IRBuilder::CreateICmpEq(Value *, Value *)
 * IRBuilder::CreateFCmpOEq(Value *, Value *)
 * IRBuilder::CreateICmpNE(Value *, Value *)
 * IRBuilder::CreateFCmpONE(Value *, Value *)
 * IRBuilder::CreateICmpSLT(Value *, Value *)
 * IRBuilder::CreateFCmpOLT(Value *, Value *)
 * IRBuilder::CreateICmpSLE(Value *, Value *)
 * IRBuilder::CreateFCmpOLE(Value *, Value *)
 * IRBuilder::CreateICmpSGT(Value *, Value *)
 * IRBuilder::CreateFCmpOGT(Value *, Value *)
 * IRBuilder::CreateICmpSGE(Value *, Value *)
 * IRBuilder::CreateFCmpOGE(Value *, Value *)
 */
struct sem_rec* rel(const char* op, struct sem_rec* x, struct sem_rec* y) {
    x = as_rvalue(x); y = as_rvalue(y);
    Value* L = (Value*) x->s_value;
    Value* R = (Value*) y->s_value;
    int Lt = x->s_type, Rt = y->s_type;
    unify(L, Lt, R, Rt);

    Value* cmp = nullptr;
    if (isDoubleTy(Lt)) {
        if (strcmp(op,"==")==0) cmp = Builder.CreateFCmpOEQ(L,R);
        else if (strcmp(op,"!=")==0) cmp = Builder.CreateFCmpONE(L,R);
        else if (strcmp(op,"<=")==0) cmp = Builder.CreateFCmpOLE(L,R);
        else if (strcmp(op,">=")==0) cmp = Builder.CreateFCmpOGE(L,R);
        else if (strcmp(op,"<")==0)  cmp = Builder.CreateFCmpOLT(L,R);
        else if (strcmp(op,">")==0)  cmp = Builder.CreateFCmpOGT(L,R);
        else { fprintf(stderr,"bad rel op\n"); exit(1);}
    } else {
        if (strcmp(op,"==")==0) cmp = Builder.CreateICmpEQ(L,R);
        else if (strcmp(op,"!=")==0) cmp = Builder.CreateICmpNE(L,R);
        else if (strcmp(op,"<=")==0) cmp = Builder.CreateICmpSLE(L,R);
        else if (strcmp(op,">=")==0) cmp = Builder.CreateICmpSGE(L,R);
        else if (strcmp(op,"<")==0)  cmp = Builder.CreateICmpSLT(L,R);
        else if (strcmp(op,">")==0)  cmp = Builder.CreateICmpSGT(L,R);
        else { fprintf(stderr,"bad rel op\n"); exit(1);}
    }
    return make_cond(cmp);
}


/*
 * cast - cast value to a different type
 *
 * LLVM API calls:
 * IRBuilder::CreateSIToFP(Value *, Type *)
 * IRBuilder::CreateFPToSI(Value *, Type *)
 */
struct sem_rec* cast(struct sem_rec* y, int t) {
    y = as_rvalue(y);
    Value* v = (Value*) y->s_value;
    int from = y->s_type;
    if ((from & ~(T_ARRAY|T_ADDR)) == (t & ~(T_ARRAY|T_ADDR))) {
        return s_node((void*)v, t & ~(T_ADDR|T_ARRAY));
    }
    Value* cv = cast_value(v, from, t);
    return s_node((void*)cv, (t & ~(T_ARRAY|T_ADDR)));
}


/*
 * assign - assignment operators
 *
 * Grammar:
 * expr -> lval ASSIGN expr          { $$ = assign((char*) "",   $1, $3); }
 * expr -> lval ASSIGN_OR expr       { $$ = assign((char*) "|",  $1, $3); }
 * expr -> lval ASSIGN_XOR expr      { $$ = assign((char*) "^",  $1, $3); }
 * expr -> lval ASSIGN_AND expr      { $$ = assign((char*) "&",  $1, $3); }
 * expr -> lval ASSIGN_LSH expr      { $$ = assign((char*) "<<", $1, $3); }
 * expr -> lval ASSIGN_RSH expr      { $$ = assign((char*) ">>", $1, $3); }
 * expr -> lval ASSIGN_ADD expr      { $$ = assign((char*) "+",  $1, $3); }
 * expr -> lval ASSIGN_SUB expr      { $$ = assign((char*) "-",  $1, $3); }
 * expr -> lval ASSIGN_MUL expr      { $$ = assign((char*) "*",  $1, $3); }
 * expr -> lval ASSIGN_DIV expr      { $$ = assign((char*) "/",  $1, $3); }
 * expr -> lval ASSIGN_MOD expr      { $$ = assign((char*) "%",  $1, $3); }
 *
 * Much of the functionality in this method is abstracted to a separate method
 * used by op2, opb, and assign. The comment above op2 lists the LLVM API calls
 * for this method.
 *
 * Additional LLVM API calls:
 * IRBuilder::CreateLoad(Type, Value *)
 * IRBuilder::CreateStore(Value *, Value *)
 */
struct sem_rec* assign(const char* op, struct sem_rec* x, struct sem_rec* y)
{
    if (!(x->s_type & T_ADDR)) { fprintf(stderr,"assign: lhs not lvalue\n"); exit(1); }

    // Load current LHS value if compound op (+= etc.)
    Value* lhs_ptr = (Value*) x->s_value;
    int    lhs_t   = x->s_type & ~(T_ADDR | T_ARRAY);
    Type*  lhs_ty  = get_llvm_type(lhs_t);

    y = as_rvalue(y);
    Value* rhs = (Value*) y->s_value;
    int    rhs_t = y->s_type;

    Value* store_val = nullptr;

    if (op[0] == '\0') {
        // Simple "=": cast RHS to LHS type then store
        if (lhs_t != (rhs_t & ~(T_ADDR|T_ARRAY))) rhs = cast_value(rhs, rhs_t, lhs_t);
        store_val = rhs;
    } else {
        // Compound assign: (old_lhs <op> rhs) then cast to lhs type and store
        Value* oldv = Builder.CreateLoad(lhs_ty, lhs_ptr);
        std::pair<Value*,int> r;
        if (!strcmp(op,"|") || !strcmp(op,"^") || !strcmp(op,"&") ||
            !strcmp(op,"<<")|| !strcmp(op,">>"))
            r = do_bit_binop(op, oldv, lhs_t, rhs, rhs_t);
        else
            r = do_arith_binop(op, oldv, lhs_t, rhs, rhs_t);

        Value* comb = r.first;
        if ((r.second & ~(T_ADDR|T_ARRAY)) != lhs_t) comb = cast_value(comb, r.second, lhs_t);
        store_val = comb;
    }

    Builder.CreateStore(store_val, lhs_ptr);
    // The value of the assignment is the stored rvalue
    return s_node((void*)store_val, lhs_t);
}


/*
 * genstring - generate code for a string
 *
 * Grammar:
 * expr ->  STR                   { $$ = genstring($1); }
 *
 * LLVM API calls:
 * IRBuilder::CreateGlobalStringPtr(char *)
 */
struct sem_rec* genstring(char* s)
{
    // Convert source-escaped text -> raw bytes (strip quotes, handle \n, \t, etc.)
    char* unescaped = parse_escape_chars(s);
    if (!unescaped) {
        yyerror("invalid string literal");
        return nullptr;
    }

    // Create a private global string and returns i8* pointer
    llvm::Value* gptr = Builder.CreateGlobalStringPtr(unescaped);

    // We no longer need the heap buffer
    free(unescaped);

    // Treat string literal as an rvalue "pointer-to-bytes" (not an lvalue)
    return s_node((void*)gptr, T_STR);
}

/*
 * declare_print - internal definition to install a print routine with
 * variable arguments
 */
void declare_print()
{
    struct id_entry* entry;
    FunctionType* var_arg;
    Value* F;
    std::string fname = "print";

    /* Add print to our internal data structure */
    var_arg = FunctionType::get(IntegerType::getInt32Ty(TheContext),
                                PointerType::get(Type::getInt8Ty(TheContext), 0), true);
    F = TheModule->getOrInsertFunction(fname, var_arg).getCallee();

    entry = install(slookup(fname.c_str()), 0);
    entry->i_type = T_INT | T_PROC;
    entry->i_value = (void*)F;
}

void init_IR()
{
    TheModule = make_unique<Module>("<stdin>", TheContext);
    declare_print();
}

void emit_IR()
{
    TheModule->print(outs(), nullptr);
}
