# binary, library, object, and source directories
CXX        = g++
CC         = gcc

LIBS       = `llvm-config --libs core native --ldflags` -lpthread -ldl -lz -ltinfo -Llib -lscan 
CXXFLAGS   = -Wall -std=c++11 -DLEFTTORIGHT `llvm-config --cxxflags`
CFLAGS     = -Wall

INPUTS_DIR = inputs
OBJ_DIR    = obj
TEST_DIR   = test
COMPILERS  = clang ref_csem csem

INPUT      = input1
SRC        = $(INPUTS_DIR)/$(INPUT).c
PRINT_OBJ  = $(OBJ_DIR)/print.o

$(foreach c,$(COMPILERS),$(eval $(c)_pp  := $(TEST_DIR)/$(c)_$(INPUT).pp.c))
$(foreach c,$(COMPILERS),$(eval $(c)_bc  := $(TEST_DIR)/$(c)_$(INPUT).bc))
$(foreach c,$(COMPILERS),$(eval $(c)_ll  := $(TEST_DIR)/$(c)_$(INPUT).ll))
$(foreach c,$(COMPILERS),$(eval $(c)_asm := $(TEST_DIR)/$(c)_$(INPUT).s))
$(foreach c,$(COMPILERS),$(eval $(c)_obj := $(TEST_DIR)/$(c)_$(INPUT).o))
$(foreach c,$(COMPILERS),$(eval $(c)_out := $(TEST_DIR)/$(c)_$(INPUT).out))

HEADERS   := $(wildcard include/*.h) $(wildcard include/*.hpp)

CFILES    := src/semutil.c src/sym.c src/parse.c
COBJECTS  := $(patsubst src/%.c,obj/%.o,$(CFILES))

CPPFILES  := src/main.cpp src/sem.cpp
CPPOBJECTS:= $(patsubst src/%.cpp,obj/%.o,$(CPPFILES))

all: csem $(PRINT_OBJ)

.PHONY: clang_ll ref_csem_ll csem_ll

clang_ll:    $(clang_ll)
ref_csem_ll: $(ref_csem_ll)
csem_ll:     $(csem_ll)

$(PRINT_OBJ):
	clang -fPIE print.c -c -o $@

obj/%.o: src/%.c $(HEADERS)
	$(CC) -I./include -g -c $< -o $@ $(CFLAGS)

obj/%.o: src/%.cpp $(HEADERS)
	$(CXX) -I./include -g -c $< -o $@ $(CXXFLAGS)

csem: $(COBJECTS) $(CPPOBJECTS)
	$(CXX) $^ $(CXXFLAGS) $(LIBS) -o $@

$(clang_ll): $(SRC)
	clang -fPIE -DDEFAULT_CLANG -O0 $(SRC) -emit-llvm -c -o $(clang_bc)
	llvm-dis $(clang_bc) -o $@

$(ref_csem_ll): $(SRC)
	clang -fPIE -E $(SRC) -o $(@:.ll=.pp.c)
	sed -i '/#/d' $(@:.ll=.pp.c)
	./ref_csem < $(@:.ll=.pp.c) > $@

$(csem_ll): $(SRC) csem
	clang -fPIE -E $(SRC) -o $(@:.ll=.pp.c)
	sed -i '/#/d' $(@:.ll=.pp.c)
	./csem < $(@:.ll=.pp.c) > $@

clang_exe:    $(PRINT_OBJ) $(clang_ll)
ref_csem_exe: $(PRINT_OBJ) $(ref_csem_ll)
csem_exe:     $(PRINT_OBJ) $(csem_ll)

clang_exe ref_csem_exe csem_exe:
	llc -relocation-model=pic $($(@:_exe=)_ll) -o $($(@:_exe=)_asm)
	clang -fPIE $($(@:_exe=)_asm) -c -o $($(@:_exe=)_obj)
	clang $(PRINT_OBJ) $($(@:_exe=)_obj) -o $@

test_clean:
	rm -f clang_exe ref_csem_exe csem_exe test/*

clean: test_clean
	rm -f csem obj/*
