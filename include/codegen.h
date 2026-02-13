#ifndef CODEGEN_H
#define CODEGEN_H

#include <stdio.h>
#include "parser.h"

// generate assembly for the parsed AST into the given FILE*
void codegen(Node *node, FILE *out);

#endif // CODEGEN_H
