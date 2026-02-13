#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include "../include/lexer.h"

static Token *new_token(TokenKind kind, char *str, size_t len, int line) {
    Token *tok = calloc(1, sizeof(Token));
    tok->kind = kind;
    tok->str = str;
    tok->len = len;
    tok->line = line;
    return tok;
}

Token *tokenize(char *p) {
    Token head = {};
    Token *cur = &head;

    int line = 1;
    while (*p) {
        // newline updates
        if (*p == '\n') { line++; p++; continue; }
        // skip whitespace except newline
        if (*p && isspace((unsigned char)*p)) { p++; continue; }

        // skip // comments
        if (p[0] == '/' && p[1] == '/') {
            p += 2;
            while (*p && *p != '\n') p++;
            continue;
        }

        // skip /* */ comments
        if (p[0] == '/' && p[1] == '*') {
            p += 2;
            while (*p && !(p[0] == '*' && p[1] == '/')) {
                if (*p == '\n') line++;
                p++;
            }
            if (*p) p += 2;
            continue;
        }

        // three-char punctuators (e.g., <<=, >>=)
        if ((p[0] == '<' && p[1] == '<' && p[2] == '=') || (p[0] == '>' && p[1] == '>' && p[2] == '=')) {
            cur->next = new_token(TK_RESERVED, p, 3, line);
            cur = cur->next;
            p += 3;
            continue;
        }

        // two-char punctuators
        if ((p[0] == '=' && p[1] == '=') || (p[0] == '!' && p[1] == '=') ||
            (p[0] == '<' && p[1] == '=') || (p[0] == '>' && p[1] == '=') ||
            (p[0] == '&' && p[1] == '&') || (p[0] == '|' && p[1] == '|') ||
            (p[0] == '<' && p[1] == '<') || (p[0] == '>' && p[1] == '>') ||
            (p[0] == '+' && p[1] == '+') || (p[0] == '-' && p[1] == '-') ||
            (p[0] == '+' && p[1] == '=') || (p[0] == '-' && p[1] == '=') ||
            (p[0] == '*' && p[1] == '=') || (p[0] == '/' && p[1] == '=') ||
            (p[0] == '%' && p[1] == '=') || (p[0] == '&' && p[1] == '=') ||
            (p[0] == '|' && p[1] == '=') || (p[0] == '^' && p[1] == '=') ||
            (p[0] == '-' && p[1] == '>')) {
            cur->next = new_token(TK_RESERVED, p, 2, line);
            cur = cur->next;
            p += 2;
            continue;
        }

        // punctuators (single char)
        if (strchr(".+-*/%{};,<>=!&|^~", *p)) {
            cur->next = new_token(TK_RESERVED, p, 1, line);
            cur = cur->next;
            if (*p == '\n') line++;
            p++;
            continue;
        }

        // string literal
        if (*p == '"') {
            p++; // skip opening quote
            int start_line = line;
            // build unescaped string
            size_t cap = 64;
            char *buf = malloc(cap);
            size_t bi = 0;
            while (*p && *p != '"') {
                if (*p == '\\') {
                    p++;
                    if (*p == 'n') { if (bi + 1 >= cap) { cap *= 2; buf = realloc(buf, cap); } buf[bi++] = '\n'; p++; continue; }
                    if (*p == 't') { if (bi + 1 >= cap) { cap *= 2; buf = realloc(buf, cap); } buf[bi++] = '\t'; p++; continue; }
                    if (*p == 'r') { if (bi + 1 >= cap) { cap *= 2; buf = realloc(buf, cap); } buf[bi++] = '\r'; p++; continue; }
                    if (*p == '\\') { if (bi + 1 >= cap) { cap *= 2; buf = realloc(buf, cap); } buf[bi++] = '\\'; p++; continue; }
                    if (*p == '"') { if (bi + 1 >= cap) { cap *= 2; buf = realloc(buf, cap); } buf[bi++] = '"'; p++; continue; }
                    // unknown escape -> take char literally
                    if (bi + 1 >= cap) { cap *= 2; buf = realloc(buf, cap); }
                    buf[bi++] = *p;
                    p++;
                    continue;
                }
                if (*p == '\n') { line++; }
                if (bi + 1 >= cap) { cap *= 2; buf = realloc(buf, cap); }
                buf[bi++] = *p;
                p++;
            }
            buf[bi] = '\0';
            if (*p == '"') p++; // skip closing quote
            cur->next = new_token(TK_STR, buf, bi, start_line);
            cur = cur->next;
            continue;
        }

        // character literal (e.g., 'a' or '\n') -> return as TK_NUM with value
        if (*p == '\'') {
            p++; // skip '\''
            int start_line = line;
            int c = 0;
            if (*p == '\\') {
                p++;
                if (*p == 'n') c = '\n';
                else if (*p == 't') c = '\t';
                else if (*p == 'r') c = '\r';
                else if (*p == '\\') c = '\\';
                else c = *p;
                p++;
            } else {
                c = *p;
                p++;
            }
            if (*p == '\'') p++; // skip closing '\''
            // store numeric literal as TK_NUM
            char *q = p; // reuse pointer for token text (not used heavily for chars)
            cur->next = new_token(TK_NUM, q, 1, start_line);
            cur = cur->next;
            cur->val = c;
            continue;
        }

        if (isalpha((unsigned char)*p) || *p == '_') {
            char *q = p;
            while (isalnum((unsigned char)*p) || *p == '_') p++;
            size_t len = p - q;
            cur->next = new_token(TK_IDENT, q, len, line);
            cur = cur->next;
            continue;
        }

        if (isdigit((unsigned char)*p)) {
            char *q = p;
            long long val = strtoll(p, &p, 10);
            int suf_ll = 0;
            int suf_u = 0;
            // support integer suffixes in any order: U/u and LL/ll (also accept single L)
            while (*p) {
                if (*p == 'u' || *p == 'U') { suf_u = 1; p++; continue; }
                if ((p[0] == 'l' || p[0] == 'L') && (p[1] == 'l' || p[1] == 'L')) { suf_ll = 1; p += 2; continue; }
                if (*p == 'l' || *p == 'L') { /* single 'l' -> long, we ignore explicit single-L for literals here */ p++; continue; }
                break;
            }
            cur->next = new_token(TK_NUM, q, p - q, line);
            cur = cur->next;
            cur->val = val;
            cur->suf_ll = suf_ll;
            cur->suf_u = suf_u;
            continue;
        }

        // unknown char
        cur->next = new_token(TK_RESERVED, p, 1, line);
        cur = cur->next;
        p++;
    }

    cur->next = new_token(TK_EOF, p, 0, line);
    return head.next;
}
