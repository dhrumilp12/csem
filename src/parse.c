#include "parse.h"
#include "cc.h"
#include "scan.h"
#include "sem.h"
#include "semutil.h"
#include "sym.h"
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

// Parse Function Declerations
static void lblstmt();
static void stmts();
static struct id_entry* parse_dclr();
static void parse_dcl();

// Expr Interface Decleartions
static void* expr();
static void* cexpr();

static Token lookahead;
static Token peek;

static Token null_token()
{
    Token token;

    token.lexeme = NULL;
    token.type = NULL_TOKEN;
    token.line = 0;
    token.column = 0;

    return token;
}

static Token get_next_token()
{
    Token next_token;

    if (peek.type != NULL_TOKEN) {
        next_token = peek;
        peek = null_token();
    } else {
        next_token = get_token();
    }

    return next_token;
}

static Token peek_token()
{
    if (peek.type == NULL_TOKEN) {
        peek = get_token();
    } else {
        fprintf(stderr, "cannot peek more than one token\n");
        exit(1);
    }
    return peek;
}

static void init_lookahead()
{
    peek = null_token();
    lookahead = get_next_token();
}

static void match(TokenType type)
{
    if (lookahead.type == type) {
        lookahead = get_next_token();
    } else {
        fprintf(stderr, "error: invalid expression at `%s` on line: %d:%d\n",
                lookahead.lexeme, lookahead.line, lookahead.column);
        exit(1);
    }
}

static bool is_type_identifier(TokenType type) {
    switch (type) {
    case CHAR:
    case INT:
    case FLOAT:
    case DOUBLE:
        return 1;
        break;
    default:
        return 0;
        break;
    };
    return 0;
}

static int csemType(TokenType type) {
    switch (type) {
    case INT:
    case CHAR:
        return T_INT;
        break;
    case FLOAT:
    case DOUBLE:
        return T_DOUBLE;
        break;
    default:
        fprintf(stderr, "Error: Expected type got: %s\n", token_type_str(type));
        exit(1);
        break;
    }
}

// Parse Function Defintions
static struct id_entry* parse_dclr() {
    Token id_token, con_token;
    struct id_entry* entry;

    id_token = lookahead;
    match(IDENTIFIER);

    if (lookahead.type != LBRACKET) {
        return dclr(id_token.lexeme, 0, 1);
    }

    match(LBRACKET);
    if (lookahead.type == INT_LITERAL) {
        con_token = lookahead;
        match(INT_LITERAL);
        entry = dclr(id_token.lexeme, T_ARRAY, con_token.int_literal);
    } else {
        entry = dclr(id_token.lexeme, T_ARRAY, 1);
    }
    match(RBRACKET);

    return entry;
}

static void parse_dcl() {
    int ty;

    if (!is_type_identifier(lookahead.type)) {
        fprintf(stderr, "Error: Expected type got: %s\n", token_type_str(lookahead.type));
        exit(1);
    }

    ty = csemType(lookahead.type);
    match(lookahead.type);

    dcl(parse_dclr(), ty, ty);

    while (lookahead.type == COMMA) {
        match(COMMA);
        dcl(parse_dclr(), ty, ty);
    }
}

void parse_dcls() {
    while (is_type_identifier(lookahead.type)) {
        parse_dcl();
        match(SEMICOLON);
    }
}

void expro() {
    if (lookahead.type == SEMICOLON) {
        return;
    }
    expr();
}

void* cexpro() {
    if (lookahead.type == SEMICOLON) {
        return node(NULL, NULL, 0, NULL, n(), NULL);
    }
    return cexpr();
}

void parse_if() {
    struct sem_rec *cond, *n1;
    void *m1, *m2, *m3, *m4;

    match(IF);
    match(LPAREN);
    cond = cexpr();
    match(RPAREN);

    m1 = m();
    lblstmt();
    m2 = m();

    if (lookahead.type != ELSE) {
        doif(cond, m1, m2);
    } else {
        match(ELSE);
        n1 = n();
        m3 = m();
        lblstmt();
        m4 = m();
        doifelse(cond, m1, n1, m3, m4);
    }
}

void parse_while() {
    struct sem_rec *cond, *n1;
    void *m1, *m2, *m3;

    match(WHILE);
    match(LPAREN);
    m1 = m();
    cond = cexpr();
    match(RPAREN);

    m2 = m();
    startloopscope();
    lblstmt();

    n1 = n();
    m3 = m();
    dowhile(m1, cond, m2, n1, m3);
}

void parse_return() {
    match(RETURN);
    if (lookahead.type == SEMICOLON) {
        doret(NULL);
    } else {
        doret(expr());
    }
}

void parse_dowhile() {
    struct sem_rec *cond;
    void *m1, *m2, *m3;

    match(DO);
    m1 = m();

    startloopscope();
    lblstmt();

    match(WHILE);
    match(LPAREN);
    m2 = m();
    cond = cexpr();
    match(RPAREN);
    match(SEMICOLON);
    m3 = m();

    dodo(m1, m2, cond, m3);
}

void parse_for() {
    struct sem_rec *cond, *n1, *n2;
    void *m1, *m2, *m3, *m4;

    match(FOR);
    match(LPAREN);
    expro();
    match(SEMICOLON);

    m1 = m();
    cond = cexpro();
    match(SEMICOLON);

    m2 = m();
    expro();
    n1 = n();
    match(RPAREN);

    m3 = m();
    startloopscope();
    lblstmt();
    n2 = n();

    m4 = m();

    dofor(m1, cond, m2, n1, m3, n2, m4);
}

void parse_continue() {
    match(CONTINUE);
    match(SEMICOLON);
    docontinue();
}

void parse_break() {
    match(BREAK);
    match(SEMICOLON);
    dobreak();
}

void parse_goto() {
    Token label;

    match(GOTO);
    label = lookahead;
    match(IDENTIFIER);

    dogoto(label.lexeme);
}

void parse_block() {
    match(LBRACE);
    stmts();
    match(RBRACE);
}

void stmt() {
    switch (lookahead.type) {
        case IF:
            parse_if();
            break;
        case WHILE:
            parse_while();
            break;
        case GOTO:
            parse_goto();
            break;
        case CONTINUE:
            parse_continue();
            break;
        case BREAK:
            parse_break();
            break;
        case FOR:
            parse_for();
            break;
        case DO:
            parse_dowhile();
            break;
        case RETURN:
            parse_return();
            break;
        case SEMICOLON:
            match(SEMICOLON);
            break;
        case LBRACE:
            parse_block();
            break;
        default:
            expr();
            break;
    }
}

void lblstmt() {
    while (lookahead.type == IDENTIFIER && peek_token().type == COLON) {
        labeldcl(lookahead.lexeme);
        match(IDENTIFIER);
        match(COLON);
    }
    stmt();
}

void stmts() {
    while (lookahead.type != RBRACE) {
        lblstmt();
    }
}

void parse_arg() {
    int ty;

    if (is_type_identifier(lookahead.type)) {
        ty = csemType(lookahead.type);
        match(lookahead.type);

        dcl(parse_dclr(), ty, PARAM);
    }
}

void args() {

    if (lookahead.type == RPAREN) {
        return;
    }

    parse_arg();
    while (lookahead.type == COMMA) {
        match(COMMA);
        parse_arg();
    }
}

void fargs() {
    match(LPAREN);
    args();
    match(RPAREN);
    enterblock();
}

void extern_func() {
    struct id_entry *entry;
    char *name;
    int ty;

    ty = T_INT;
    if (is_type_identifier(lookahead.type)) {
        ty = csemType(lookahead.type);
        match(lookahead.type);

        if (peek_token().type != LPAREN) {
            dcl(parse_dclr(), ty, ty);
            while (lookahead.type == COMMA) {
                match(COMMA);
                dcl(parse_dclr(), ty, ty);
            }
            match(SEMICOLON);
            return;
        }
    }

    name = lookahead.lexeme;
    match(IDENTIFIER);

    entry = fname(ty, name);
    fargs();

    match(LBRACE);
    parse_dcls();
    fhead(entry);
    stmts();
    match(RBRACE);

    ftail();
}

void externs() {
    while (lookahead.type != EOF_TOKEN) {
        extern_func();
    }
}

void parse() {
    init_lookahead();

    enterblock();
    enterblock();
    externs();
}

// Code for EXPR 
enum ExprType {
    ETYPE_LVAL,
    ETYPE_EXPR,
    ETYPE_COND,
};

#define BINARY_PRECEDENCE_LIST                                               \
    X(ASSIGN_OR, "|=", 2, 'R', assign, ETYPE_LVAL, ETYPE_EXPR, ETYPE_EXPR)   \
    X(ASSIGN_XOR, "^=", 2, 'R', assign, ETYPE_LVAL, ETYPE_EXPR, ETYPE_EXPR)  \
    X(ASSIGN_AND, "&=", 2, 'R', assign, ETYPE_LVAL, ETYPE_EXPR, ETYPE_EXPR)  \
    X(ASSIGN_LSH, "<<=", 2, 'R', assign, ETYPE_LVAL, ETYPE_EXPR, ETYPE_EXPR) \
    X(ASSIGN_RSH, ">>=", 2, 'R', assign, ETYPE_LVAL, ETYPE_EXPR, ETYPE_EXPR) \
    X(ASSIGN_ADD, "+=", 2, 'R', assign, ETYPE_LVAL, ETYPE_EXPR, ETYPE_EXPR)  \
    X(ASSIGN_SUB, "-=", 2, 'R', assign, ETYPE_LVAL, ETYPE_EXPR, ETYPE_EXPR)  \
    X(ASSIGN_MUL, "*=", 2, 'R', assign, ETYPE_LVAL, ETYPE_EXPR, ETYPE_EXPR)  \
    X(ASSIGN_DIV, "/=", 2, 'R', assign, ETYPE_LVAL, ETYPE_EXPR, ETYPE_EXPR)  \
    X(ASSIGN_MOD, "%=", 2, 'R', assign, ETYPE_LVAL, ETYPE_EXPR, ETYPE_EXPR)  \
    X(ASSIGN, "", 2, 'R', assign, ETYPE_LVAL, ETYPE_EXPR, ETYPE_EXPR)        \
    X(ADD, "+", 12, 'L', op2, ETYPE_EXPR, ETYPE_EXPR, ETYPE_EXPR)            \
    X(SUB, "-", 12, 'L', op2, ETYPE_EXPR, ETYPE_EXPR, ETYPE_EXPR)            \
    X(MUL, "*", 13, 'L', op2, ETYPE_EXPR, ETYPE_EXPR, ETYPE_EXPR)            \
    X(DIV, "/", 13, 'L', op2, ETYPE_EXPR, ETYPE_EXPR, ETYPE_EXPR)            \
    X(MOD, "%", 13, 'L', op2, ETYPE_EXPR, ETYPE_EXPR, ETYPE_EXPR)            \
    X(BIT_XOR, "^", 7, 'L', op2, ETYPE_EXPR, ETYPE_EXPR, ETYPE_EXPR)         \
    X(BIT_AND, "&", 8, 'L', op2, ETYPE_EXPR, ETYPE_EXPR, ETYPE_EXPR)         \
    X(BIT_OR, "|", 6, 'L', op2, ETYPE_EXPR, ETYPE_EXPR, ETYPE_EXPR)          \
    X(LT, "<", 10, 'L', rel, ETYPE_EXPR, ETYPE_EXPR, ETYPE_COND)             \
    X(LE, "<=", 10, 'L', rel, ETYPE_EXPR, ETYPE_EXPR, ETYPE_COND)            \
    X(GT, ">", 10, 'L', rel, ETYPE_EXPR, ETYPE_EXPR, ETYPE_COND)             \
    X(GE, ">=", 10, 'L', rel, ETYPE_EXPR, ETYPE_EXPR, ETYPE_COND)            \
    X(AND, "", 5, 'L', rel, ETYPE_COND, ETYPE_COND, ETYPE_COND)              \
    X(OR, "", 4, 'L', rel, ETYPE_COND, ETYPE_COND, ETYPE_COND)               \
    X(EQ, "=", 9, 'L', rel, ETYPE_EXPR, ETYPE_EXPR, ETYPE_COND)              \
    X(NE, "!", 9, 'L', rel, ETYPE_EXPR, ETYPE_EXPR, ETYPE_COND)

struct op_info {
    const char* op_string;
    int precedence;
    char assoc;
    void* (*function)(const char*, void*, void*);
    enum ExprType left_type;
    enum ExprType right_type;
    enum ExprType ret_type;
};

static int get_op_info(TokenType ty, struct op_info* info) {
    switch (ty) {
#define X(FIRST, SECOND, THIRD, FOURTH, FITH, SIXTH, SEVENTH, EIGTH)                                                        \
    case FIRST:                                                                                                             \
        *info = (struct op_info){SECOND, THIRD, FOURTH, (void* (*)(const char*, void*, void*))FITH, SIXTH, SEVENTH, EIGTH}; \
        break;

        BINARY_PRECEDENCE_LIST
#undef X
        default:
            return 0;
    }
    return 1;
}

struct expression {
    void *val;
    enum ExprType type;
};

static struct expression compute_expr(int);
static struct expression enforce_expr_type(struct expression ex, enum ExprType ty);

void *add_expr(void *cur_exprs) {
    Token token;
    void *next_expr;

    if (lookahead.type == STRING_LITERAL) {
        token = lookahead;
        match(STRING_LITERAL);
        next_expr = genstring(token.lexeme);
    } else {
        next_expr = expr();
    }

    return (cur_exprs == NULL) ? next_expr
                               : exprs(cur_exprs, next_expr);
}

void *parse_exprs() {
    void *cur_exprs;

    if (lookahead.type == RPAREN) {
        return NULL;
    }

    cur_exprs = add_expr(NULL);
    while (lookahead.type == COMMA) {
        match(COMMA);
        cur_exprs = add_expr(cur_exprs);
    }
    return cur_exprs;
}

static struct expression handle_atom_id(Token tk) {
    struct expression ex;
    void *indx_expr;

    if (lookahead.type == LPAREN) {
        match(LPAREN);
        call(tk.lexeme, parse_exprs());
        match(RPAREN);
        ex.type = ETYPE_EXPR;
    } else if (lookahead.type == LBRACKET) {
        match(LBRACKET);
        indx_expr = expr();
        match(RBRACKET);
        ex.val = indx(id(tk.lexeme), indx_expr);
        ex.type = ETYPE_LVAL;
    } else {
        ex.val = id(tk.lexeme);
        ex.type = ETYPE_LVAL;
    }
    return ex;
}

static struct expression get_atom() {
    struct expression ex;
    Token token;

    token = lookahead;
    switch (token.type) {
    case INT_LITERAL:
        match(INT_LITERAL);
        ex.val = con(token.int_literal);
        ex.type = ETYPE_EXPR;
        return ex;
    case LPAREN:
        match(LPAREN);
        ex = compute_expr(0);
        match(RPAREN);
        return ex;
    case SUB:
        match(SUB);
        ex = enforce_expr_type(get_atom(), ETYPE_EXPR);
        ex.val = op1("-", ex.val);
        ex.type = ETYPE_EXPR;
        return ex;
    case BIT_NOT:
        match(BIT_NOT);
        ex = enforce_expr_type(get_atom(), ETYPE_EXPR);
        ex.val = op1("~", ex.val);
        ex.type = ETYPE_EXPR;
        return ex;
    case IDENTIFIER:
        match(IDENTIFIER);
        return handle_atom_id(token);
    case NOT:
        match(NOT);
        ex.val = ccnot(cexpr());
        ex.type = ETYPE_COND;
        return ex;
    default:
        fprintf(stderr, "unimplemented case in get_atom %s", token_type_str(token.type));
        exit(1);
    }
}

static struct expression enforce_expr_type(struct expression ex, enum ExprType ty) {

    if (ty == ETYPE_EXPR) {
        if (ex.type == ETYPE_LVAL) {
            ex.val = op1("@", ex.val);
        } else if (ex.type == ETYPE_COND) {
            fprintf(stderr, "unimplemented case in enforce_expr_type\n");
            exit(1);
        }
        ex.type = ETYPE_EXPR;

    } else if (ty == ETYPE_COND) {
        if (ex.type == ETYPE_LVAL) {
            ex.val = op1("@", ex.val);
        } else if (ex.type == ETYPE_EXPR) {
            ex.val = ccexpr(ex.val);
        }
        ex.type = ETYPE_COND;

    } else { // ty == ETYPE_LVAL
        if (ex.type != ETYPE_LVAL) {
            fprintf(stderr, "Expected LVAL\n");
            exit(1);
        }
    }

    return ex;
}

static struct expression compute_expr(int min_prec) {
    struct expression left, right;
    struct op_info opi;
    void *m1;
    int next_prec;
    Token optok;

    left = get_atom();

    while (get_op_info(lookahead.type, &opi) && opi.precedence >= min_prec) {
        optok = lookahead;
        match(lookahead.type);
        left = enforce_expr_type(left, opi.left_type);

        next_prec = opi.precedence;
        if (opi.assoc == 'L') {
            next_prec += 1;
        }

        if (optok.type == AND || optok.type == OR) {
            m1 = m();
        }

        right = compute_expr(next_prec);
        right = enforce_expr_type(right, opi.right_type);

        if (optok.type == AND) {
            left.val = ccand(left.val, m1, right.val);
        } else if (optok.type == OR) {
            left.val = ccor(left.val, m1, right.val);
        } else {
            left.val = opi.function(opi.op_string, left.val, right.val);
        }

        left.type = opi.ret_type;
    }

    return left;
}

void* expr() {
    struct expression ex = enforce_expr_type(compute_expr(0), ETYPE_EXPR);
    return ex.val;
}

void* cexpr() {
    struct expression ex = enforce_expr_type(compute_expr(0), ETYPE_COND);
    return ex.val;
}
