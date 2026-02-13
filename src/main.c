#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "../include/lexer.h"
#include "../include/parser.h"
#include "../include/codegen.h"

static char *read_file(const char *path) {
    FILE *f = fopen(path, "rb");
    if (!f) return NULL;
    fseek(f, 0, SEEK_END);
    long sz = ftell(f);
    fseek(f, 0, SEEK_SET);
    char *buf = malloc(sz + 1);
    if (!buf) { fclose(f); return NULL; }
    if (fread(buf, 1, sz, f) != (size_t)sz) { fclose(f); free(buf); return NULL; }
    buf[sz] = '\0';
    fclose(f);
    return buf;
}

#include "options.h"

int opt_verbose = 0;

void print_node_rec(Node *m, int depth) {
    if (!m) return;
    if (m->kind == ND_SEQ) { print_node_rec(m->lhs, depth+1); print_node_rec(m->rhs, depth+1); return; }
    for (int i = 0; i < depth; i++) fprintf(stderr, "  ");
    fprintf(stderr, "node kind=%d name=%s is_const=%d is_global=%d\n", m->kind, m->name ? m->name : "-", m->is_const, m->is_global);
}

int main(int argc, char **argv) {
    int dump_tokens = 0;
    // parse flags and positional args (simple parser)
    const char *input_path = NULL;
    const char *output_path = NULL;
    for (int i = 1; i < argc; i++) {
        if (strcmp(argv[i], "--dump-tokens") == 0) dump_tokens = 1;
        else if (strcmp(argv[i], "-v") == 0 || strcmp(argv[i], "--verbose") == 0) opt_verbose = 1;
        else {
            if (!input_path) input_path = argv[i];
            else if (!output_path) output_path = argv[i];
        }
    }

    if (!dump_tokens && (!input_path || !output_path)) {
        fprintf(stderr, "usage: jcc [--dump-tokens] [-v|--verbose] input.c output.s\n");
        return 1;
    }

    const char *read_path = dump_tokens ? input_path : input_path;
    char *input = read_file(read_path);
    if (!input) {
        perror("read input");
        return 1;
    }

    Token *toks = tokenize(input);
    if (dump_tokens) {
        for (Token *t = toks; t; t = t->next) {
            fprintf(stderr, "kind=%d len=%zu line=%d str=\"%.*s\" val=%lld\n", t->kind, t->len, t->line, (int)t->len, t->str ? t->str : "", t->val);
        }
        return 0;
    }
    Node *root = parse(toks);

    // debug: list parsed nodes
    if (opt_verbose) {
        for (Node *n = root; n; n = n->rhs) {
            if (n->kind == ND_FUNC) fprintf(stderr, "[debug] func '%s' defined=%d\n", n->name, n->body != NULL);
        }
        for (Node *n = root; n; n = n->rhs) {
            fprintf(stderr, "[AST] kind=%d name=%s is_global=%d has_init=%d\n", n->kind, n->name ? n->name : "?", n->is_global, n->lhs ? 1 : 0);
            if (n->kind == ND_FUNC && n->body) {
                fprintf(stderr, "[AST BODY of %s]:\n", n->name);
                // recursive print of sequence
                print_node_rec(n->body, 0);
            }
        }
    }

    // run type checker
    type_check(root);

    FILE *out = fopen(output_path, "w");
    if (!out) { perror("open output"); return 1; }
    // generate code for the whole top-level AST (codegen handles sequences)
    codegen(root, out);
    fclose(out);
    return 0;
}
