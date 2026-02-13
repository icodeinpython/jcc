#include <stdio.h>
#include <stdlib.h>
#include "../include/lexer.h"

int main(int argc, char **argv) {
    if (argc < 2) { fprintf(stderr, "usage: lexdump file\n"); return 1; }
    FILE *f = fopen(argv[1], "r"); if (!f) { perror("fopen"); return 1; }
    fseek(f, 0, SEEK_END);
    long sz = ftell(f);
    fseek(f, 0, SEEK_SET);
    char *buf = malloc(sz + 1);
    fread(buf, 1, sz, f);
    buf[sz] = '\0';
    Token *t = tokenize(buf);
    for (Token *it = t; it; it = it->next) {
        printf("kind=%d len=%zu line=%d str='%.*s' val=%lld suf_ll=%d suf_u=%d\n", it->kind, it->len, it->line, (int)it->len, it->str?it->str:"", it->val, it->suf_ll, it->suf_u);
    }
    return 0;
}
