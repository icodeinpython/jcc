#ifndef LEXER_H
#define LEXER_H

#include <stddef.h>

typedef enum {
    TK_RESERVED, // punctuation or operators
    TK_IDENT,    // identifier or keyword
    TK_NUM,      // integer literal
    TK_STR,      // string literal
    TK_EOF,
} TokenKind;

typedef struct Token Token;

struct Token {
    TokenKind kind;
    Token *next;
    long long val;      // if kind == TK_NUM (store full 64-bit value)
    int suf_ll;         // non-zero if literal had LL/ll suffix
    int suf_u;          // non-zero if literal had U/u suffix (unsigned)
    char *str;    // token string (null-terminated)
    size_t len;   // length of token string
    int line;     // source line where token begins
};

// tokenize input string and return head token
Token *tokenize(char *p);

#endif // LEXER_H
