#ifndef PARSER_H
#define PARSER_H

#include "lexer.h"
#include "type.h"

typedef enum {
    ND_NUM,
    ND_ADD,
    ND_SUB,
    ND_MUL,
    ND_DIV,
    ND_MOD,
    ND_RETURN,
    ND_VAR,    // variable reference
    ND_ASSIGN, // assignment expression
    ND_DECL,   // variable declaration
    ND_SEQ,    // sequence of statements
    ND_EQ,     // ==
    ND_NE,     // !=
    ND_LT,     // <
    ND_LE,     // <=
    ND_LAND,   // &&
    ND_LOR,    // ||
    ND_NOT,    // !
    ND_BITNOT, // ~
    ND_BITAND, // &
    ND_BITXOR, // ^
    ND_BITOR,  // |
    ND_SHL,    // <<
    ND_SHR,    // >>
    ND_DEREF,  // *ptr
    ND_ADDR,   // &var
    ND_CALL,   // function call
    ND_IF,
    ND_WHILE,
    ND_DO,
    ND_STR,    // string literal (char*)
    ND_SIZEOF, // sizeof operator
    ND_FUNC,
    ND_SWITCH,
    ND_CASE,
    ND_DEFAULT,
    ND_BREAK,
    ND_CONTINUE,
    ND_INIT, // initializer list
    ND_LABEL, // label (name: stmt)
    ND_GOTO,  // goto name;
} NodeKind;

typedef struct Node Node;

struct Node {
    NodeKind kind;
    Node *lhs;
    Node *rhs;
    Type *ty;       // node type
    long long val;        // for ND_NUM (64-bit)
    char *name;     // for ND_FUNC
    Node *body;     // function body (or sequence)
    Node *els;      // else branch for if
    Node *params;   // parameter list for functions (rhs-chained ND_DECL)
    int line;        // source line for node (for diagnostics)
    char *str;       // string contents for ND_STR
    int is_global;   // set for top-level declarations
    int is_static;   // storage class
    int is_register; // 'register' storage-class hint
    int is_extern;   // extern storage-class (declaration without definition)
    int is_const;    // const qualifier
    int is_restrict; // 'restrict' (pointer) qualifier
    char *glabel;    // generated global label (for static locals or renamed globals)
};

// parse a token stream (from tokenize) into an AST root (function node)
Node *parse(Token *tok);

#endif // PARSER_H
