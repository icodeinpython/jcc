#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "../include/parser.h"
#include "../include/lexer.h"

static Token *tok;

static int token_eq(Token *t, const char *s) {
    size_t len = strlen(s);
    return t->kind == TK_IDENT && t->len == len && memcmp(t->str, s, len) == 0;
}

// typedef-name table for parser-level typedefs
typedef struct TDef TDef;
struct TDef { char *name; Type *ty; TDef *next; };
static TDef *typedefs = NULL;

static void typedef_add(const char *name, Type *ty) {
    TDef *t = calloc(1, sizeof(TDef));
    t->name = strdup(name);
    t->ty = ty;
    t->next = typedefs;
    typedefs = t;
}

static Type *typedef_lookup(Token *t) {
    if (t->kind != TK_IDENT) return NULL;
    for (TDef *it = typedefs; it; it = it->next) if (t->len == strlen(it->name) && memcmp(t->str, it->name, t->len) == 0) return it->ty;
    return NULL;
}

static void error(const char *msg) {
    fprintf(stderr, "parse error: %s\n", msg);
    if (tok) {
        fprintf(stderr, "near token: '%.*s' (kind=%d len=%zu)\n", (int)tok->len, tok->str ? tok->str : "", tok->kind, tok->len);
    }
    exit(1);
}

static Node *new_node(NodeKind kind, Node *lhs, Node *rhs) {
    Node *n = calloc(1, sizeof(Node));
    n->kind = kind;
    n->lhs = lhs;
    n->rhs = rhs;
    return n;
}

static Node *new_num(int val) {
    Node *n = calloc(1, sizeof(Node));
    n->kind = ND_NUM;
    n->val = val;
    return n;
}

static void advance() { if (tok) tok = tok->next; }

// forward declarations
static Node *expr();
static Node *assign();
static Node *equality();
static Node *relational();

static Node *stmt();

// create a sequence node: left ; right
static Node *new_seq(Node *a, Node *b) {
    if (!a) return b;
    if (!b) return a;
    Node *n = calloc(1, sizeof(Node));
    n->kind = ND_SEQ;
    n->lhs = a;
    n->rhs = b;
    return n;
}

// shallow-clone helper for lvalue nodes (ND_VAR, ND_DEREF)
static Node *clone_node(Node *n) {
    if (!n) return NULL;
    Node *c = calloc(1, sizeof(Node));
    c->kind = n->kind;
    c->ty = n->ty;
    c->line = n->line;
    if (n->kind == ND_VAR) { c->name = strdup(n->name); return c; }
    if (n->kind == ND_DEREF) { c->lhs = clone_node(n->lhs); return c; }
    // other kinds are not expected for lvalues in our usage
    return NULL;
}

static Node *primary() {
    if (tok->kind == TK_NUM) {
        Node *n = new_num(tok->val);
        advance();
        return n;
    }

    if (tok->kind == TK_STR) {
        Node *n = calloc(1, sizeof(Node));
        n->kind = ND_STR;
        n->str = strndup(tok->str, tok->len);
        advance();
        return n;
    }

    if (tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == '(') {
        advance();
        Node *n = expr();
        if (!(tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == ')'))
            error("expected )");
        advance();
        return n;
    }
    if (tok->kind == TK_IDENT) {
        // function call or variable reference
        char *name = strndup(tok->str, tok->len);
        advance();
        if (tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == '(') {
            // call
            advance();
            Node *args_head = NULL;
            Node *args_tail = NULL;
            if (!(tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == ')')) {
                for (;;) {
                    Node *a = expr();
                    // chain args via rhs
                    if (!args_head) args_head = args_tail = a;
                    else { args_tail->rhs = a; args_tail = a; }
                    if (tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == ',') { advance(); continue; }
                    break;
                }
            }
            if (!(tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == ')')) error("expected ) after call");
            advance();
            Node *n = calloc(1, sizeof(Node));
            n->kind = ND_CALL;
            n->name = name;
            n->lhs = args_head; // args in rhs-chain
            return n;
        }
        // variable reference
        Node *n = calloc(1, sizeof(Node));
        n->kind = ND_VAR;
        n->name = name;
        
        // handle postfix operators: array-indexing and post-inc/dec
        for (;;) {
            if (tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == '[') {
                // a[index]  ->  * (a + index)
                advance();
                Node *idx = expr();
                if (!(tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == ']')) error("expected ] after index");
                advance();
                Node *add = new_node(ND_ADD, n, idx);
                n = calloc(1, sizeof(Node));
                n->kind = ND_DEREF;
                n->lhs = add;
                continue; // allow multiple indexing like a[0][1]
            }
            if (tok->kind == TK_RESERVED && tok->len == 2 && tok->str[0] == '+' && tok->str[1] == '+') {
                // post-increment: produce sequence (orig, assign(orig, orig+1))
                Node *orig = clone_node(n);
                Node *lhs = clone_node(n);
                Node *rhs = new_node(ND_ADD, clone_node(n), new_num(1));
                Node *assign = new_node(ND_ASSIGN, lhs, rhs);
                Node *seq = new_seq(orig, assign);
                advance();
                return seq;
            }
            if (tok->kind == TK_RESERVED && tok->len == 2 && tok->str[0] == '-' && tok->str[1] == '-') {
                // post-decrement
                Node *orig = clone_node(n);
                Node *lhs = clone_node(n);
                Node *rhs = new_node(ND_SUB, clone_node(n), new_num(1));
                Node *assign = new_node(ND_ASSIGN, lhs, rhs);
                Node *seq = new_seq(orig, assign);
                advance();
                return seq;
            }
            break;
        }

        return n;
    }
    error("expected a number, identifier or (expr)");
    return NULL;
}

static Type *parse_type_specifier();

static Node *unary() {
    // prefix ++ / --
    if (tok->kind == TK_RESERVED && tok->len == 2 && tok->str[0] == '+' && tok->str[1] == '+') {
        advance();
        Node *v = unary();
        Node *lhs = clone_node(v);
        Node *rhs = new_node(ND_ADD, clone_node(v), new_num(1));
        return new_node(ND_ASSIGN, lhs, rhs);
    }
    if (tok->kind == TK_RESERVED && tok->len == 2 && tok->str[0] == '-' && tok->str[1] == '-') {
        advance();
        Node *v = unary();
        Node *lhs = clone_node(v);
        Node *rhs = new_node(ND_SUB, clone_node(v), new_num(1));
        return new_node(ND_ASSIGN, lhs, rhs);
    }

    // unary ops: + - ! ~ & * sizeof
    if (tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == '+') {
        advance();
        return unary();
    }
    if (tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == '-') {
        advance();
        return new_node(ND_SUB, new_num(0), unary());
    }
    if (tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == '!') {
        advance();
        return new_node(ND_NOT, unary(), NULL);
    }
    if (tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == '~') {
        advance();
        return new_node(ND_BITNOT, unary(), NULL);
    }
    if (tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == '&') {
        advance();
        return new_node(ND_ADDR, unary(), NULL);
    }
    if (tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == '*') {
        advance();
        return new_node(ND_DEREF, unary(), NULL);
    }
    if (token_eq(tok, "sizeof")) {
        advance();
        // support sizeof(type) as well as sizeof expr
        if (tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == '(') {
            advance();
            Type *t = parse_type_specifier();
            if (t) {
                if (!(tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == ')')) error("expected )");
                advance();
                Node *n = calloc(1, sizeof(Node));
                n->kind = ND_SIZEOF;
                Node *tmp = calloc(1, sizeof(Node));
                tmp->ty = t;
                n->lhs = tmp;
                return n;
            }
            // not a type-name: parse sizeof(expression)
            Node *n = calloc(1, sizeof(Node));
            n->kind = ND_SIZEOF;
            Node *exprn = expr();
            if (!(tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == ')')) error("expected )");
            advance();
            n->lhs = exprn;
            return n;
        }
        Node *n = calloc(1, sizeof(Node));
        n->kind = ND_SIZEOF;
        n->lhs = unary();
        return n;
    }
    return primary();
}

static Node *mul() {
    Node *node = unary();
    for (;;) {
        if (tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == '*') {
            advance();
            node = new_node(ND_MUL, node, unary());
            continue;
        }
        if (tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == '/') {
            advance();
            node = new_node(ND_DIV, node, unary());
            continue;
        }
        if (tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == '%') {
            advance();
            node = new_node(ND_MOD, node, unary());
            continue;
        }
        break;
    }
    return node;
}

static Node *add() {
    Node *node = mul();
    for (;;) {
        if (tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == '+') {
            advance();
            node = new_node(ND_ADD, node, mul());
            continue;
        }
        if (tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == '-') {
            advance();
            node = new_node(ND_SUB, node, mul());
            continue;
        }
        break;
    }
    return node;
}

static Node *shift() {
    Node *node = add();
    for (;;) {
        if (tok->kind == TK_RESERVED && tok->len == 2 && tok->str[0] == '<' && tok->str[1] == '<') {
            advance();
            node = new_node(ND_SHL, node, add());
            continue;
        }
        if (tok->kind == TK_RESERVED && tok->len == 2 && tok->str[0] == '>' && tok->str[1] == '>') {
            advance();
            node = new_node(ND_SHR, node, add());
            continue;
        }
        break;
    }
    return node;
}

static Node *relational() {
    Node *node = shift();
    for (;;) {
        if (tok->kind == TK_RESERVED && tok->len == 2 && tok->str[0] == '<' && tok->str[1] == '=') {
            advance();
            node = new_node(ND_LE, node, shift());
            continue;
        }
        if (tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == '<') {
            advance();
            node = new_node(ND_LT, node, shift());
            continue;
        }
        if (tok->kind == TK_RESERVED && tok->len == 2 && tok->str[0] == '>' && tok->str[1] == '=') {
            advance();
            // a >= b  ->  b <= a
            node = new_node(ND_LE, shift(), node);
            continue;
        }
        if (tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == '>') {
            advance();
            // a > b -> b < a
            node = new_node(ND_LT, shift(), node);
            continue;
        }
        break;
    }
    return node;
}

static Node *equality() {
    Node *node = relational();
    for (;;) {
        if (tok->kind == TK_RESERVED && tok->len == 2 && tok->str[0] == '=' && tok->str[1] == '=') {
            advance();
            node = new_node(ND_EQ, node, relational());
            continue;
        }
        if (tok->kind == TK_RESERVED && tok->len == 2 && tok->str[0] == '!' && tok->str[1] == '=') {
            advance();
            node = new_node(ND_NE, node, relational());
            continue;
        }
        break;
    }
    return node;
}

static Node *bit_and() {
    Node *node = equality();
    for (;;) {
        if (tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == '&') {
            advance();
            node = new_node(ND_BITAND, node, equality());
            continue;
        }
        break;
    }
    return node;
}

static Node *bit_xor() {
    Node *node = bit_and();
    for (;;) {
        if (tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == '^') {
            advance();
            node = new_node(ND_BITXOR, node, bit_and());
            continue;
        }
        break;
    }
    return node;
}

static Node *bit_or() {
    Node *node = bit_xor();
    for (;;) {
        if (tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == '|') {
            advance();
            node = new_node(ND_BITOR, node, bit_xor());
            continue;
        }
        break;
    }
    return node;
}

static Node *logical_and() {
    Node *node = bit_or();
    for (;;) {
        if (tok->kind == TK_RESERVED && tok->len == 2 && tok->str[0] == '&' && tok->str[1] == '&') {
            advance();
            node = new_node(ND_LAND, node, bit_or());
            continue;
        }
        break;
    }
    return node;
}

static Node *logical_or() {
    Node *node = logical_and();
    for (;;) {
        if (tok->kind == TK_RESERVED && tok->len == 2 && tok->str[0] == '|' && tok->str[1] == '|') {
            advance();
            node = new_node(ND_LOR, node, logical_and());
            continue;
        }
        break;
    }
    return node;
}

static Node *assign() {
    Node *node = logical_or();
    if (tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == '=') {
        advance();
        Node *rhs = assign();
        return new_node(ND_ASSIGN, node, rhs);
    }

    // compound-assignment: transform 'a += b' -> 'a = a + b'
    if (tok->kind == TK_RESERVED && tok->len == 2) {
        Node *rhs_expr = NULL;
        if (tok->str[0] == '+' && tok->str[1] == '=') { advance(); rhs_expr = new_node(ND_ADD, clone_node(node), assign()); return new_node(ND_ASSIGN, node, rhs_expr); }
        if (tok->str[0] == '-' && tok->str[1] == '=') { advance(); rhs_expr = new_node(ND_SUB, clone_node(node), assign()); return new_node(ND_ASSIGN, node, rhs_expr); }
        if (tok->str[0] == '*' && tok->str[1] == '=') { advance(); rhs_expr = new_node(ND_MUL, clone_node(node), assign()); return new_node(ND_ASSIGN, node, rhs_expr); }
        if (tok->str[0] == '/' && tok->str[1] == '=') { advance(); rhs_expr = new_node(ND_DIV, clone_node(node), assign()); return new_node(ND_ASSIGN, node, rhs_expr); }
        if (tok->str[0] == '%' && tok->str[1] == '=') { advance(); rhs_expr = new_node(ND_MOD, clone_node(node), assign()); return new_node(ND_ASSIGN, node, rhs_expr); }
        if (tok->str[0] == '&' && tok->str[1] == '=') { advance(); rhs_expr = new_node(ND_BITAND, clone_node(node), assign()); return new_node(ND_ASSIGN, node, rhs_expr); }
        if (tok->str[0] == '|' && tok->str[1] == '=') { advance(); rhs_expr = new_node(ND_BITOR, clone_node(node), assign()); return new_node(ND_ASSIGN, node, rhs_expr); }
        if (tok->str[0] == '^' && tok->str[1] == '=') { advance(); rhs_expr = new_node(ND_BITXOR, clone_node(node), assign()); return new_node(ND_ASSIGN, node, rhs_expr); }
    }
    // handle '<<=' and '>>=' which lexer returns as len==3 tokens
    if (tok->kind == TK_RESERVED && tok->len == 3) {
        if (tok->str[0] == '<' && tok->str[1] == '<' && tok->str[2] == '=') { advance(); Node *rhs_expr = new_node(ND_SHL, clone_node(node), assign()); return new_node(ND_ASSIGN, node, rhs_expr); }
        if (tok->str[0] == '>' && tok->str[1] == '>' && tok->str[2] == '=') { advance(); Node *rhs_expr = new_node(ND_SHR, clone_node(node), assign()); return new_node(ND_ASSIGN, node, rhs_expr); }
    }
    return node;
}
static Node *expr() {
    return assign();
}

// statement: either declaration, expression statement, or return
static Node *stmt() {
    if (tok) fprintf(stderr, "[parser.stmt] at token='%.*s' kind=%d\n", (int)tok->len, tok->str?tok->str:"", tok->kind);
    if (token_eq(tok, "return")) {
        advance();
        Node *e = expr();
        if (!(tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == ';')) error("expected ';' after return");
        advance();
        Node *n = calloc(1, sizeof(Node));
        n->kind = ND_RETURN;
        n->lhs = e;
        return n;
    }

    if (token_eq(tok, "if")) {
        advance();
        if (!(tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == '(')) error("expected ( after if");
        advance();
        Node *cond = expr();
        if (!(tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == ')')) error("expected ) after if condition");
        advance();
        Node *then = stmt();
        Node *els = NULL;
        if (token_eq(tok, "else")) {
            advance();
            els = stmt();
        }
        Node *n = calloc(1, sizeof(Node));
        n->kind = ND_IF;
        n->lhs = cond;
        n->rhs = then;
        n->els = els;
        return n;
    }

    if (token_eq(tok, "while")) {
        advance();
        if (!(tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == '(')) error("expected ( after while");
        advance();
        Node *cond = expr();
        if (!(tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == ')')) error("expected ) after while condition");
        advance();
        Node *body = stmt();
        Node *n = calloc(1, sizeof(Node));
        n->kind = ND_WHILE;
        n->lhs = cond;
        n->rhs = body;
        return n;
    }

    // for-loop
    if (token_eq(tok, "for")) {
        advance();
        if (!(tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == '(')) error("expected ( after for");
        advance();
        // init: declaration or expression or empty
        Node *init = NULL;
        if (!(tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == ';')) {
            // reuse stmt() to parse init (it will consume the trailing ';')
            init = stmt();
        } else {
            // consume ';'
            advance();
        }
        // condition
        Node *cond = NULL;
        if (!(tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == ';')) {
            cond = expr();
        }
        if (!(tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == ';')) error("expected ; after for condition");
        advance();
        // increment (expression) or empty
        Node *inc = NULL;
        if (!(tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == ')')) {
            inc = expr();
        }
        if (!(tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == ')')) error("expected ) after for");
        advance();
        Node *body = stmt();
        // desugar: for(init; cond; inc) body  =>  init; while(cond) { body; inc; }
        if (!cond) cond = new_num(1);
        Node *loop_body = inc ? new_seq(body, inc) : body;
        Node *whilen = calloc(1, sizeof(Node)); whilen->kind = ND_WHILE; whilen->lhs = cond; whilen->rhs = loop_body;
        return init ? new_seq(init, whilen) : whilen;
    }

    // block
    if (tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == '{') {
        advance();
        Node *seq = NULL;
        while (!(tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == '}')) {
            Node *s = stmt();
            seq = new_seq(seq, s);
        }
        advance(); // consume '}'
        return seq;
    }

    // labels
    if (tok->kind == TK_IDENT && tok->next && tok->next->kind == TK_RESERVED && tok->next->len == 1 && tok->next->str[0] == ':') {
        char *name = strndup(tok->str, tok->len);
        advance(); advance(); // consume IDENT ':'
        Node *s = stmt();
        Node *n = calloc(1, sizeof(Node)); n->kind = ND_LABEL; n->name = name; n->lhs = s; return n;
    }

    // break / continue / goto
    if (token_eq(tok, "break")) { advance(); if (!(tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == ';')) error("expected ';' after break"); advance(); Node *n = calloc(1, sizeof(Node)); n->kind = ND_BREAK; return n; }
    if (token_eq(tok, "continue")) { advance(); if (!(tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == ';')) error("expected ';' after continue"); advance(); Node *n = calloc(1, sizeof(Node)); n->kind = ND_CONTINUE; return n; }
    if (token_eq(tok, "goto")) { advance(); if (tok->kind != TK_IDENT) error("expected label name after goto"); Node *n = calloc(1, sizeof(Node)); n->kind = ND_GOTO; n->name = strndup(tok->str, tok->len); advance(); if (!(tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == ';')) error("expected ';' after goto"); advance(); return n; }

    // switch
    if (token_eq(tok, "switch")) {
        advance();
        if (!(tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == '(')) error("expected ( after switch");
        advance();
        Node *cond = expr();
        if (!(tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == ')')) error("expected ) after switch condition");
        advance();
        if (!(tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == '{')) error("expected '{' after switch");
        advance();
        Node *seq = NULL;
        while (!(tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == '}')) {
            if (token_eq(tok, "case")) {
                advance();
                Node *val = expr();
                if (!(tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == ':')) error("expected : after case");
                advance();
                Node *body = stmt();
                Node *c = calloc(1, sizeof(Node)); c->kind = ND_CASE; c->lhs = val; c->rhs = body; seq = new_seq(seq, c);
                continue;
            }
            if (token_eq(tok, "default")) {
                advance();
                if (!(tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == ':')) error("expected : after default");
                advance();
                Node *body = stmt();
                Node *d = calloc(1, sizeof(Node)); d->kind = ND_DEFAULT; d->rhs = body; seq = new_seq(seq, d);
                continue;
            }
            Node *s = stmt();
            seq = new_seq(seq, s);
        }
        advance(); // consume '}'
        Node *n = calloc(1, sizeof(Node)); n->kind = ND_SWITCH; n->lhs = cond; n->rhs = seq; return n;
    }

    // do-while
    if (token_eq(tok, "do")) {
        advance();
        Node *body = stmt();
        if (!token_eq(tok, "while")) error("expected while after do");
        advance();
        if (!(tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == '(')) error("expected ( after while");
        advance();
        Node *cond = expr();
        if (!(tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == ')')) error("expected ) after while condition");
        advance();
        if (!(tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == ';')) error("expected ; after do-while");
        advance();
        Node *n = calloc(1, sizeof(Node)); n->kind = ND_DO; n->lhs = cond; n->rhs = body; return n;
    }

    // local typedef
    if (token_eq(tok, "typedef")) {
        advance();
        // typedef enum { ... } NAME;  (local)
        if (token_eq(tok, "enum")) {
            advance();
            if (tok->kind == TK_IDENT) advance();
            Node *seq = NULL;
            if (tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == '{') {
                advance();
                long next_val = 0;
                while (!(tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == '}')) {
                    if (tok->kind != TK_IDENT) error("expected enumerator name");
                    char *ename = strndup(tok->str, tok->len);
                    advance();
                    Node *n = calloc(1, sizeof(Node)); n->kind = ND_DECL; n->name = ename; n->is_const = 1; n->ty = ty_int;
                    if (tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == '=') { advance(); n->lhs = expr(); if (n->lhs && n->lhs->kind == ND_NUM) next_val = n->lhs->val + 1; else next_val++; }
                    else { n->lhs = new_num((int)next_val); next_val++; }
                    seq = new_seq(seq, n);
                    fprintf(stderr, "[parser] local enum enumerator parsed: %s\n", ename);
                    if (tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == ',') { advance(); continue; }
                    break;
                }
                if (!(tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == '}')) error("expected } in enum");
                advance();
            }
            // typedef name
            if (tok->kind != TK_IDENT) error("expected identifier in typedef");
            typedef_add(strndup(tok->str, tok->len), ty_int);
            advance();
            if (!(tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == ';')) error("expected ; after typedef");
            advance();
            return seq; // may be NULL if no enumerators
        }
        // general typedef (local)
        Type *ty = parse_type_specifier();
        if (!ty) error("expected type in typedef");
        while (1) {
            if (tok->kind != TK_IDENT) error("expected identifier in typedef");
            typedef_add(strndup(tok->str, tok->len), ty);
            advance();
            if (tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == ',') { advance(); continue; }
            break;
        }
        if (!(tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == ';')) error("expected ; after typedef");
        advance();
        return NULL;
    }

    // declaration: allow optional storage-class/qualifier and a base type
    if (token_eq(tok, "register") || token_eq(tok, "const") || token_eq(tok, "char") || token_eq(tok, "int") || token_eq(tok, "unsigned") || token_eq(tok, "long") || token_eq(tok, "_Bool") || token_eq(tok, "enum") || token_eq(tok, "struct") || typedef_lookup(tok)) {
        // handle local enum declarations like 'enum { X = 1, Y };'
        if (token_eq(tok, "enum")) {
            advance();
            if (tok->kind == TK_IDENT) advance();
            if (tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == '{') {
                advance();
                Node *seq = NULL;
                long next_val = 0;
                while (!(tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == '}')) {
                    if (tok->kind != TK_IDENT) error("expected enumerator name");
                    char *ename = strndup(tok->str, tok->len);
                    advance();
                    Node *n = calloc(1, sizeof(Node)); n->kind = ND_DECL; n->name = ename; n->is_const = 1; n->ty = ty_int;
                    if (tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == '=') {
                        advance(); n->lhs = expr(); if (n->lhs && n->lhs->kind == ND_NUM) next_val = n->lhs->val + 1; else next_val++;
                    } else { n->lhs = new_num((int)next_val); next_val++; }
                    seq = new_seq(seq, n);
                    if (tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == ',') { advance(); continue; }
                    break;
                }
                if (!(tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == '}')) error("expected } in enum");
                advance();
                if (!(tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == ';')) error("expected ; after enum");
                advance();
                return seq;
            }
            // otherwise fall through to normal declaration handling (enum as type-name)
        }

        int is_register = 0;
        int is_const = 0;
        int is_restrict = 0;
        Type *base_ty = NULL;
        if (token_eq(tok, "register")) { is_register = 1; advance(); }
        if (token_eq(tok, "const")) { is_const = 1; advance(); }
        // parse base type (int/char/long/unsigned/_Bool)
        base_ty = parse_type_specifier();
        if (!base_ty) error("expected a type in declaration");

        // optional pointer declarator (parse_type_specifier may already have consumed '*')
        int ptr = 0;
        if (base_ty && base_ty->kind == TY_PTR) { ptr = 1; base_ty = base_ty->base; if (token_eq(tok, "restrict")) { is_restrict = 1; advance(); } }
        else if (tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == '*') {
            ptr = 1; advance();
            if (token_eq(tok, "restrict")) { is_restrict = 1; advance(); }
        }

        if (tok->kind != TK_IDENT) error("expected identifier in declaration");
        Node *n = calloc(1, sizeof(Node));
        n->kind = ND_DECL;
        n->name = strndup(tok->str, tok->len);
        n->is_register = is_register;
        n->is_const = is_const;
        n->is_restrict = is_restrict;
        advance();

        // array declarator: identifier '[' number ']'
        if (tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == '[') {
            advance();
            if (tok->kind != TK_NUM) error("expected array length");
            size_t alen = (size_t)tok->val;
            advance();
            if (!(tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == ']')) error("expected ] after array length");
            advance();
            n->ty = array_of(base_ty, alen);
        } else if (ptr) {
            // set pointer type for simple case
            n->ty = pointer_to(base_ty);
        } else {
            // non-pointer, non-array: set base type
            n->ty = base_ty;
        }

        if (tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == '=') {
            advance();
            // support initializer-list for arrays or expression initializer
            if (tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == '{') {
                advance();
                Node *init_head = NULL;
                Node *init_tail = NULL;
                while (!(tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == '}')) {
                    Node *e = expr();
                    if (!init_head) init_head = init_tail = e; else { init_tail->rhs = e; init_tail = e; }
                    if (tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == ',') { advance(); continue; }
                    break;
                }
                if (!(tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == '}')) error("expected } in initializer");
                advance();
                Node *in = calloc(1, sizeof(Node)); in->kind = ND_INIT; in->lhs = init_head; n->lhs = in;
            } else {
                n->lhs = expr(); // initializer expression
            }
        }
        if (!(tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == ';')) error("expected ';' after declaration");
        advance();
        return n;
    }

    // expression statement
    Node *e = expr();
    if (!(tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == ';')) error("expected ';' after expression");
    advance();
    return e;
}

// helper: parse base type specifiers (int/char/unsigned/long/long long/_Bool/struct/typedef-names)
static Type *parse_type_specifier() {
    // struct
    if (token_eq(tok, "struct")) {
        advance();
        Type *st = NULL;
        char *tag = NULL;
        if (tok->kind == TK_IDENT) { tag = strndup(tok->str, tok->len); advance(); st = struct_named_type(tag); }
        // definition: struct TAG { ... }
        if (tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == '{') {
            advance();
            Member *members_head = NULL, *members_tail = NULL;
            while (!(tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == '}')) {
                Type *mty = parse_type_specifier();
                if (!mty) error("expected type in struct member");
                // expect identifier for member name
                if (tok->kind != TK_IDENT) error("expected member name");
                Member *m = calloc(1, sizeof(Member));
                m->name = strndup(tok->str, tok->len);
                advance();
                // optional array declarator
                if (tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == '[') {
                    advance();
                    if (tok->kind != TK_NUM) error("expected array length");
                    size_t alen = (size_t)tok->val;
                    advance();
                    if (!(tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == ']')) error("expected ] after array length");
                    advance();
                    m->ty = array_of(mty, alen);
                } else {
                    m->ty = mty;
                }
                if (!(tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == ';')) error("expected ; after member");
                advance();
                if (!members_head) members_head = members_tail = m; else { members_tail->next = m; members_tail = m; }
            }
            advance(); // consume '}'
            if (!st) { st = calloc(1, sizeof(Type)); st->kind = TY_STRUCT; st->size = 0; }
            struct_define(st, members_head);
            return st;
        }
        if (!st) { Type *anon = calloc(1, sizeof(Type)); anon->kind = TY_STRUCT; anon->size = 0; return anon; }
        return st;
    }

    // _Bool
    if (token_eq(tok, "_Bool")) { advance(); return ty_bool; }

    // unsigned/short/long/int/char handling
    int is_unsigned = 0;
    int long_count = 0;

    if (token_eq(tok, "unsigned")) { is_unsigned = 1; advance(); }
    if (token_eq(tok, "signed")) { /* treat as signed */ advance(); }
    if (token_eq(tok, "short")) { advance(); return is_unsigned ? ty_ushort : ty_short; }

    // consume 0..2 'long's
    while (token_eq(tok, "long")) { long_count++; advance(); if (long_count == 2) break; }

    Type *base = NULL;
    if (token_eq(tok, "char")) { advance(); base = is_unsigned ? ty_uchar : ty_char; }
    else if (tok->kind == TK_IDENT && tok->len == 3 && memcmp(tok->str, "int", 3) == 0) {
        advance();
        if (long_count == 2) base = is_unsigned ? ty_ulonglong : ty_longlong;
        else if (long_count == 1) base = is_unsigned ? ty_ulong : ty_long;
        else base = is_unsigned ? ty_uint : ty_int;
    } else if (long_count > 0) {
        // 'long' or 'long long' without explicit int
        if (long_count == 2) base = is_unsigned ? ty_ulonglong : ty_longlong;
        else base = is_unsigned ? ty_ulong : ty_long;
    } else if (is_unsigned) {
        base = ty_uint; // 'unsigned' alone
    } else {
        // maybe this is a typedef-name
        Type *td = typedef_lookup(tok);
        if (td) { advance(); return td; }
        return NULL;
    }

    // accept simple pointer declarator in type-name context (e.g. 'char*' inside sizeof(...))
    while (tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == '*') {
        advance();
        base = pointer_to(base);
    }
    return base;
}


// parse a function: <type> IDENT() { <stmts> }
Node *parse(Token *t) {
    tok = t;
    Node *top = NULL;
    Node *tail = NULL;
    while (tok && tok->kind != TK_EOF) {
        // handle top-level 'typedef'
        if (token_eq(tok, "typedef")) {
            advance();
            fprintf(stderr, "[parser.parse] after 'typedef' token='%.*s' kind=%d\n", (int)tok->len, tok->str?tok->str:"", tok->kind);
            // special-case typedef enum { ... } NAME;
            if (token_eq(tok, "enum")) {
                advance();
                if (tok->kind == TK_IDENT) advance();
                if (tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == '{') {
                    advance();
                    long next_val = 0;
                    while (!(tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == '}')) {
                        if (tok->kind != TK_IDENT) error("expected enumerator name");
                        char *ename = strndup(tok->str, tok->len);
                        advance();
                        Node *n = calloc(1, sizeof(Node)); n->kind = ND_DECL; n->name = ename; n->is_const = 1; n->is_global = 1; n->ty = ty_int;
                        if (tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == '=') { advance(); n->lhs = expr(); if (n->lhs && n->lhs->kind == ND_NUM) next_val = n->lhs->val + 1; else next_val++; }
                        else { n->lhs = new_num((int)next_val); next_val++; }
                        if (!top) top = tail = n; else { tail->rhs = n; tail = n; }
                        if (tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == ',') { advance(); continue; }
                        break;
                    }
                    if (!(tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == '}')) error("expected } in enum");
                    advance();
                }
                // now expect typedef name(s)
                Type *ty = ty_int;
                while (1) {
                    if (tok->kind != TK_IDENT) error("expected identifier in typedef");
                    typedef_add(strndup(tok->str, tok->len), ty);
                    advance();
                    if (tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == ',') { advance(); continue; }
                    break;
                }
                if (!(tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == ';')) error("expected ';' after typedef");
                advance();
                continue;
            }
            Type *ty = parse_type_specifier();
            if (!ty) error("expected type in typedef");
            // expect identifier(s)
            while (1) {
                if (tok->kind != TK_IDENT) error("expected identifier in typedef");
                typedef_add(strndup(tok->str, tok->len), ty);
                advance();
                if (tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == ',') { advance(); continue; }
                break;
            }
            if (!(tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == ';')) error("expected ';' after typedef");
            advance();
            continue;
        }

        // handle top-level enums (emit enumerator constants as const ND_DECLs)
        if (token_eq(tok, "enum")) {
            advance();
            // optional tag
            if (tok->kind == TK_IDENT) advance();
            if (tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == '{') {
                advance();
                long next_val = 0;
                while (!(tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == '}')) {
                    if (tok->kind != TK_IDENT) error("expected enumerator name");
                    char *ename = strndup(tok->str, tok->len);
                    advance();
                    Node *n = calloc(1, sizeof(Node)); n->kind = ND_DECL; n->name = ename; n->is_const = 1; n->is_global = 1; n->ty = ty_int;
                    if (tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == '=') {
                        advance();
                        Node *v = expr(); n->lhs = v; // allow expression; codegen will eval
                        // try to fold simple constant to update next_val where possible
                        if (v && v->kind == ND_NUM) next_val = v->val + 1; else next_val++;
                    } else { n->lhs = new_num((int)next_val); next_val++; }
                    // append to top-level
                    if (!top) top = tail = n; else { tail->rhs = n; tail = n; }
                    if (tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == ',') { advance(); continue; }
                    break;
                }
                if (!(tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == '}')) error("expected } after enum");
                advance();
                if (!(tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == ';')) error("expected ; after enum");
                advance();
                continue;
            }
            // otherwise treat 'enum' as type-name in a declaration; fall through to normal handling
        }

        // storage-class for top-level declarations
        int is_extern = 0;
        int is_static = 0;
        if (token_eq(tok, "extern")) { is_extern = 1; advance(); }
        if (token_eq(tok, "static")) { is_static = 1; advance(); }

        // parse a base type for either a function or a global declaration
        Type *base_ty = parse_type_specifier();
        if (!base_ty) error("expected return type at start of function or declaration");

        // allow tag-only struct declarations like 'struct S { ... };'
        if (tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == ';' && base_ty->kind == TY_STRUCT) {
            advance();
            continue;
        }

        // expect identifier (function name or variable name)
        if (tok->kind != TK_IDENT) error("expected function or variable name");
        char *name = strndup(tok->str, tok->len);
        advance();

        // function: '(' after name
        if (tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == '(') {
            Node *fn = calloc(1, sizeof(Node));
            fn->kind = ND_FUNC;
            fn->name = name;
            fn->ty = base_ty;
            advance();

            // parse parameters
            Node *params_head = NULL; Node *params_tail = NULL;
            if (!(tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == ')')) {
                for (;;) {
                    int p_is_register = 0; int p_is_const = 0; int p_is_restrict = 0; int ptr = 0;
                    if (token_eq(tok, "register")) { p_is_register = 1; advance(); }
                    if (token_eq(tok, "const")) { p_is_const = 1; advance(); }
                    Type *pty = parse_type_specifier(); if (!pty) error("expected type in parameter");
                    // parse_type_specifier may already have consumed a trailing '*'
                    if (pty->kind == TY_PTR) { ptr = 1; pty = pty->base; if (token_eq(tok, "restrict")) { p_is_restrict = 1; advance(); } }
                    else if (tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == '*') { ptr = 1; advance(); if (token_eq(tok, "restrict")) { p_is_restrict = 1; advance(); } }
                    Node *p = calloc(1, sizeof(Node)); p->kind = ND_DECL; p->is_register = p_is_register; p->is_const = p_is_const; p->is_restrict = p_is_restrict; if (ptr) p->ty = pointer_to(pty); else p->ty = pty; 
                    if (tok->kind == TK_IDENT) { p->name = strndup(tok->str, tok->len); advance(); }
                    if (!params_head) params_head = params_tail = p; else { params_tail->rhs = p; params_tail = p; }
                    if (tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == ',') { advance(); continue; }
                    break;
                }
            }
            if (!(tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == ')')) error("expected )");
            advance();

            // prototype
            if (tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == ';') {
                advance(); fn->params = params_head; fn->body = NULL; if (!top) top = tail = fn; else { tail->rhs = fn; tail = fn; } continue;
            }
            // definition
            if (!(tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == '{')) error("expected '{' for function body");
            advance(); Node *seq = NULL; while (!(tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == '}')) { Node *s = stmt(); seq = new_seq(seq, s); } advance();
            // debug: dump the immediate children of the function body
            for (Node *it = seq; it; it = it->rhs) fprintf(stderr, "[parser] func '%s' child kind=%d name=%s is_const=%d\n", name, it->kind, it->name?it->name:"-", it->is_const);
            fn->params = params_head; fn->body = seq; if (!top) top = tail = fn; else { tail->rhs = fn; tail = fn; } continue;
        }

        // otherwise: one-or-more global declarators (handle arrays/initializers)
        while (1) {
            Node *n = calloc(1, sizeof(Node)); n->kind = ND_DECL; n->name = strdup(name); n->is_global = 1; n->is_static = is_static; n->is_extern = is_extern; n->ty = base_ty;
            // array declarator for globals: name '[' num ']'
            if (tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == '[') {
                advance(); if (tok->kind != TK_NUM) error("expected array length"); size_t alen = (size_t)tok->val; advance(); if (!(tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == ']')) error("expected ]"); advance(); n->ty = array_of(base_ty, alen);
            }
            // initializer
            if (tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == '=') {
                advance();
                if (tok->kind == TK_STR) { Node *s = calloc(1, sizeof(Node)); s->kind = ND_STR; s->str = strndup(tok->str, tok->len); n->lhs = s; advance(); }
                else if (tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == '{') {
                    advance(); Node *init_head = NULL; Node *init_tail = NULL; while (!(tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == '}')) { Node *e = expr(); if (!init_head) init_head = init_tail = e; else { init_tail->rhs = e; init_tail = e; } if (tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == ',') { advance(); continue; } break; } if (!(tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == '}')) error("expected } in initializer"); advance(); Node *in = calloc(1, sizeof(Node)); in->kind = ND_INIT; in->lhs = init_head; n->lhs = in; }
                else { n->lhs = expr(); }
            }

            if (!top) top = tail = n; else { tail->rhs = n; tail = n; }

            if (tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == ',') {
                // more declarators of same base type
                advance(); if (tok->kind != TK_IDENT) error("expected identifier in declaration"); name = strndup(tok->str, tok->len); advance(); continue;
            }
            break;
        }

        if (!(tok->kind == TK_RESERVED && tok->len == 1 && tok->str[0] == ';')) error("expected ';' after declaration");
        advance();
    }

    return top;
}
