#pragma once

#ifdef __cplusplus
extern "C"{
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define TOKEN_VALUES        \
    X(ASSIGN, "=")          \
    X(ASSIGN_OR, "|=")      \
    X(ASSIGN_XOR, "^=")     \
    X(ASSIGN_AND, "&=")     \
    X(ASSIGN_LSH, "<<=")    \
    X(ASSIGN_RSH, ">>=")    \
    X(ASSIGN_ADD, "+=")     \
    X(ASSIGN_SUB, "-=")     \
    X(ASSIGN_MUL, "*=")     \
    X(ASSIGN_DIV, "/=")     \
    X(ASSIGN_MOD, "%=")     \
    X(OR, "||")             \
    X(AND, "&&")            \
    X(BIT_NOT, "~")         \
    X(BIT_OR, "|")          \
    X(BIT_XOR, "^")         \
    X(BIT_AND, "&")         \
    X(EQ, "==")             \
    X(NE, "!=")             \
    X(GT, ">")              \
    X(GE, ">=")             \
    X(LT, "<")              \
    X(LE, "<=")             \
    X(LSH, "<<")            \
    X(RSH, ">>")            \
    X(ADD, "+")             \
    X(SUB, "-")             \
    X(MUL, "*")             \
    X(DIV, "/")             \
    X(MOD, "%")             \
    X(NOT, "!")             \
    X(SEMICOLON, ";")       \
    X(COLON, ":")           \
    X(COMMA, ",")           \
    X(LPAREN, "(")          \
    X(RPAREN, ")")          \
    X(LBRACKET, "[")        \
    X(RBRACKET, "]")        \
    X(LBRACE, "{")          \
    X(RBRACE, "}")          \
    X(IF, "if")             \
    X(FOR, "for")           \
    X(ELSE, "else")         \
    X(WHILE, "while")       \
    X(DOUBLE, "double")     \
    X(DO, "do")             \
    X(RETURN, "return")     \
    X(CONTINUE, "continue") \
    X(BREAK, "break")       \
    X(GOTO, "goto")         \
    X(CHAR, "char")         \
    X(FLOAT, "float")       \
    X(INT, "int")

typedef enum TokenType {
#define X(FIRST, SECOND) FIRST,
   TOKEN_VALUES 
#undef X
    INT_LITERAL,
    REAL_LITERAL,
    STRING_LITERAL,
    IDENTIFIER,
    EOF_TOKEN,
    NULL_TOKEN,
} TokenType;

typedef struct Token {
    TokenType type;
    int line;
    int column;
    char* lexeme;
    union {
        double float_literal;
        long long int_literal;
    };
} Token;

typedef struct Keyword {
    TokenType type;
    char *lexeme;
    struct Keyword *next;
} Keyword;

static void print_escaped_char(char c) {
	switch (c) {
	case '\n': printf("\\n"); break;
	case '\t': printf("\\t"); break;
	case '\r': printf("\\r"); break;
	case '\v': printf("\\v"); break;
	case '\f': printf("\\f"); break;
	case '\a': printf("\\a"); break;
	case '\b': printf("\\b"); break;
	case '\\': printf("\\\\"); break;
	case '\"': printf("\\\""); break;
	case '\'': printf("\\\'"); break;
	case '\0': printf("\\0"); break;
	default:
		if ((unsigned char)c < 32 || (unsigned char)c > 126) {
			// Non-printable -> show as hex escape
			printf("\\x%02x", (unsigned char)c);
		} else {
			// Printable character
			printf("%c", c);
		}
		break;
	}
}

static void print_escaped_str(char *s) {
    while (*s) {
        print_escaped_char(*s++);
    }
}

static const char* token_type_str(TokenType ty) {
    switch (ty) {
#define X(FIRST, SECOND) \
    case FIRST:          \
        return #FIRST;
   TOKEN_VALUES 
#undef X
    case INT_LITERAL:
       return "INT_LITERAL";
    case REAL_LITERAL:
       return "REAL_LITERAL";
    case STRING_LITERAL:
       return "STRING_LITERAL";
    case IDENTIFIER:
       return "IDENTIFIER";
    case EOF_TOKEN:
       return "EOF";
    default:
       return "Unhandled token name";
    }
}

static void print_token(Token token) {
    switch (token.type) {
#define X(FIRST, SECOND) \
    case FIRST: 
        TOKEN_VALUES
#undef X
    case IDENTIFIER:
            printf("%-16s %s", token_type_str(token.type), token.lexeme);
            printf("\n");
            break;
    case STRING_LITERAL:
            printf("%-16s \"", token_type_str(token.type));
            print_escaped_str(token.lexeme);
            printf("\" (length=%ld)", strlen(token.lexeme));
            printf("\n");
            break;
    case INT_LITERAL:
            printf("%-16s %lld\n", token_type_str(token.type),
                   token.int_literal);
            break;
    case REAL_LITERAL:
            printf("%-16s %f\n", token_type_str(token.type),
                   token.float_literal);
            break;
    default:
            printf("Unimplemented Print %d",token.type);
            exit(1);
            break;
    }
}


// IMPLEMENT THESE
void init_scanner();
Token get_token();

#ifdef __cplusplus
}
#endif
