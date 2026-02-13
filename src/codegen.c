#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "../include/codegen.h"
#include "../include/options.h"

// simple linked list of local variables
typedef struct LVar LVar;
struct LVar {
    LVar *next;
    char *name;
    int offset; // offset from RBP
    Type *ty;
};

// global variable table (top-level globals)
typedef struct GVar GVar;
struct GVar { GVar *next; char *name; Type *ty; int is_static; int is_extern; };
static GVar *gvars = NULL;
static int labelseq = 0;

// label table for current function
typedef struct Label Label;
struct Label { Label *next; char *name; char *asmname; };
static Label *labels = NULL;

static char *label_get_asmname(char *name) {
    for (Label *l = labels; l; l = l->next) if (strcmp(l->name, name) == 0) return l->asmname;
    Label *l = calloc(1, sizeof(Label));
    l->name = strdup(name);
    char buf[64]; sprintf(buf, ".L%d", labelseq++);
    l->asmname = strdup(buf);
    l->next = labels; labels = l;
    return l->asmname;
} 

// return 1 if this call should emit storage for the variable; return 0 to skip emission
static int gvar_push(char *name, Type *ty, int is_static, int is_extern) {
    for (GVar *g = gvars; g; g = g->next) {
        if (strcmp(g->name, name) == 0) {
            if (g->is_extern && !is_extern) {
                // previously declared as an extern -> now we have a definition. update and allow emission.
                g->ty = ty ? ty : ty_int;
                g->is_static = is_static;
                g->is_extern = 0;
                return 1;
            }
            // already exists and is not a replaceable extern -> skip emission
            return 0;
        }
    }
    GVar *g = calloc(1, sizeof(GVar));
    g->name = strdup(name);
    g->ty = ty ? ty : ty_int;
    g->is_static = is_static;
    g->is_extern = is_extern;
    g->next = gvars; gvars = g;
    // if this is a non-defining extern, don't emit now
    return is_extern ? 0 : 1;
}

static GVar *gvar_find(char *name) {
    for (GVar *g = gvars; g; g = g->next) if (strcmp(g->name, name) == 0) return g;
    return NULL;
}

static LVar *locals;
static int stack_size;
static int returned;
static Type *cur_func_ret = NULL;

// helpers for size-aware loads/stores (all results are left in %%rax/%%eax appropriately)
static int cg_is_unsigned(Type *t) {
    if (!t) return 0;
    return t->kind == TY_UINT || t->kind == TY_UCHAR || t->kind == TY_USHORT || t->kind == TY_ULONG || t->kind == TY_ULONGLONG || t->kind == TY_BOOL;
}

static void load_from_stack_offset(int offset, Type *ty, FILE *out) {
    int s = (int)size_of(ty);
    if (s == 8) fprintf(out, "\tmovq -%d(%%rbp), %%rax\n", offset);
    else if (s == 4) fprintf(out, "\tmovl -%d(%%rbp), %%eax\n", offset);
    else if (s == 2) {
        if (cg_is_unsigned(ty)) fprintf(out, "\tmovzwl -%d(%%rbp), %%eax\n", offset);
        else fprintf(out, "\tmovswl -%d(%%rbp), %%eax\n", offset);
    } else if (s == 1) {
        if (cg_is_unsigned(ty)) fprintf(out, "\tmovzbq -%d(%%rbp), %%rax\n", offset);
        else fprintf(out, "\tmovsbq -%d(%%rbp), %%rax\n", offset);
    } else { fprintf(stderr, "unsupported load size %d\n", s); exit(1); }
}

static void store_to_stack_offset(int offset, Type *ty, FILE *out) {
    int s = (int)size_of(ty);
    if (s == 8) fprintf(out, "\tmovq %%rax, -%d(%%rbp)\n", offset);
    else if (s == 4) fprintf(out, "\tmovl %%eax, -%d(%%rbp)\n", offset);
    else if (s == 2) fprintf(out, "\tmovw %%ax, -%d(%%rbp)\n", offset);
    else if (s == 1) fprintf(out, "\tmovb %%al, -%d(%%rbp)\n", offset);
    else {
        fprintf(stderr, "unsupported store size %d at offset %d\n", s, offset);
        for (LVar *lv = locals; lv; lv = lv->next) {
            fprintf(stderr, "  local %s offset=%d size=%zu kind=%d\n", lv->name, lv->offset, lv->ty ? lv->ty->size : 0, lv->ty ? (int)lv->ty->kind : -1);
        }
        exit(1);
    }
}

static void load_from_address_in_rax(Type *ty, FILE *out) {
    int s = (int)size_of(ty);
    if (s == 8) fprintf(out, "\tmovq (%%rax), %%rax\n");
    else if (s == 4) fprintf(out, "\tmovl (%%rax), %%eax\n");
    else if (s == 2) {
        if (cg_is_unsigned(ty)) fprintf(out, "\tmovzwl (%%rax), %%eax\n");
        else fprintf(out, "\tmovswl (%%rax), %%eax\n");
    } else if (s == 1) {
        if (cg_is_unsigned(ty)) fprintf(out, "\tmovzbq (%%rax), %%rax\n");
        else fprintf(out, "\tmovsbq (%%rax), %%rax\n");
    } else { fprintf(stderr, "unsupported deref size %d\n", s); exit(1); }
}

static void store_to_address_in_rcx(Type *ty, FILE *out) {
    int s = (int)size_of(ty);
    if (s == 8) fprintf(out, "\tmovq %%rax, (%%rcx)\n");
    else if (s == 4) fprintf(out, "\tmovl %%eax, (%%rcx)\n");
    else if (s == 2) fprintf(out, "\tmovw %%ax, (%%rcx)\n");
    else if (s == 1) fprintf(out, "\tmovb %%al, (%%rcx)\n");
    else {
        fprintf(stderr, "unsupported store size %d for address store (ty kind=%d)\n", s, ty ? (int)ty->kind : -1);
        for (LVar *lv = locals; lv; lv = lv->next) {
            fprintf(stderr, "  local %s offset=%d size=%zu kind=%d\n", lv->name, lv->offset, lv->ty ? lv->ty->size : 0, lv->ty ? (int)lv->ty->kind : -1);
        }
        exit(1);
    }
}

static void load_global_into_rax(char *name, Type *ty, FILE *out) {
    int s = (int)size_of(ty);
    if (s == 8) fprintf(out, "\tmovq %s(%%rip), %%rax\n", name);
    else if (s == 4) fprintf(out, "\tmovl %s(%%rip), %%eax\n", name);
    else if (s == 2) {
        if (cg_is_unsigned(ty)) fprintf(out, "\tmovzwl %s(%%rip), %%eax\n", name);
        else fprintf(out, "\tmovswl %s(%%rip), %%eax\n", name);
    } else if (s == 1) {
        if (cg_is_unsigned(ty)) fprintf(out, "\tmovzbq %s(%%rip), %%rax\n", name);
        else fprintf(out, "\tmovsbq %s(%%rip), %%rax\n", name);
    } else { fprintf(stderr, "unsupported global load size %d\n", s); exit(1); }
}

static void store_rax_to_global(char *name, Type *ty, FILE *out) {
    int s = (int)size_of(ty);
    if (s == 8) fprintf(out, "\tmovq %%rax, %s(%%rip)\n", name);
    else if (s == 4) fprintf(out, "\tmovl %%eax, %s(%%rip)\n", name);
    else if (s == 2) fprintf(out, "\tmovw %%ax, %s(%%rip)\n", name);
    else if (s == 1) fprintf(out, "\tmovb %%al, %s(%%rip)\n", name);
    else { fprintf(stderr, "unsupported global store size %d\n", s); exit(1); }
}

static void add_lvar(char *name, Type *ty) {
    // check exists
    for (LVar *v = locals; v; v = v->next) {
        if (strcmp(v->name, name) == 0) return;
    }
    LVar *v = calloc(1, sizeof(LVar));
    v->name = strdup(name);
    v->ty = ty ? ty : ty_int;
    // allocate space based on type size, round up to 8 for alignment
    size_t s = size_of(v->ty);
    int alloc = ((s + 7) / 8) * 8;
    if (alloc == 0) alloc = 8;
    stack_size += alloc;
    v->offset = stack_size;
    v->next = locals;
    locals = v;
}

static LVar *find_lvar(char *name) {
    for (LVar *v = locals; v; v = v->next) {
        if (strcmp(v->name, name) == 0) return v;
    }
    return NULL;
}

static int strseq = 0;
static int break_label_stack[128];
static int break_sp = 0;
static char *continue_label_stack[128];
static int continue_sp = 0;
static char *emit_string_literal(FILE *out, char *s);

// forward prototypes for helpers used below
static void gen(Node *node, FILE *out);
static int eval_const_int(Node *n, long *out);

// helpers for switch support
static void switch_collect_counts(Node *n, int *case_count, int *default_exists) {
    if (!n) return;
    if (n->kind == ND_SEQ) { switch_collect_counts(n->lhs, case_count, default_exists); switch_collect_counts(n->rhs, case_count, default_exists); return; }
    if (n->kind == ND_CASE) (*case_count)++;
    if (n->kind == ND_DEFAULT) (*default_exists) = 1;
}
static void switch_collect_fill(Node *n, Node **cases, int *ci, char **default_label) {
    if (!n) return;
    if (n->kind == ND_SEQ) { switch_collect_fill(n->lhs, cases, ci, default_label); switch_collect_fill(n->rhs, cases, ci, default_label); return; }
    if (n->kind == ND_CASE) {
        char buf[32]; sprintf(buf, ".Lcase%d", labelseq++); n->glabel = strdup(buf); cases[(*ci)++] = n; return;
    }
    if (n->kind == ND_DEFAULT) { char buf[32]; sprintf(buf, ".Lcase%d", labelseq++); n->glabel = strdup(buf); *default_label = n->glabel; return; }
}
static void switch_gen_body(Node *n, FILE *out) {
    if (!n) return;
    if (n->kind == ND_SEQ) { switch_gen_body(n->lhs, out); switch_gen_body(n->rhs, out); return; }
    if (n->kind == ND_CASE) { fprintf(out, "%s:\n", n->glabel); if (n->rhs) gen(n->rhs, out); return; }
    if (n->kind == ND_DEFAULT) { fprintf(out, "%s:\n", n->glabel); if (n->rhs) gen(n->rhs, out); return; }
    // normal statement
    gen(n, out);
}

// helpers to place a label before the last statement of a sequence (used for for-loop's 'inc' placement)
// find the first statement of the rightmost subtree (i.e., the first stmt in the trailing 'inc' sequence)
static Node *find_first_of_tail(Node *n) {
    if (!n) return NULL;
    Node *tail = n;
    while (tail->kind == ND_SEQ && tail->rhs) tail = tail->rhs; // rightmost subtree root
    Node *first = tail;
    while (first->kind == ND_SEQ && first->lhs) first = first->lhs; // leftmost within that subtree
    return first;
}
static int node_contains(Node *root, Node *target) {
    if (!root) return 0;
    if (root == target) return 1;
    if (root->kind == ND_SEQ) return node_contains(root->lhs, target) || node_contains(root->rhs, target);
    return 0;
}
static int node_contains_inc_pattern(Node *root) {
    if (!root) return 0;
    Node *stack[128]; int sp = 0; stack[sp++] = root;
    while (sp > 0) {
        Node *cur = stack[--sp]; if (!cur) continue;
        if (cur->kind == ND_ASSIGN && cur->lhs && cur->lhs->kind == ND_VAR && cur->rhs && cur->rhs->kind == ND_ADD) {
            Node *a = cur->rhs->lhs, *b = cur->rhs->rhs;
            if (a && b && ((a->kind == ND_VAR && b->kind == ND_NUM && b->val == 1) || (b->kind == ND_VAR && a->kind == ND_NUM && a->val == 1))) return 1;
        }
        if (cur->kind == ND_SEQ) { if (cur->rhs) stack[sp++] = cur->rhs; if (cur->lhs) stack[sp++] = cur->lhs; }
    }
    return 0;
}
static void gen_prefix_until(Node *cur, Node *stop, FILE *out) {
    if (!cur) return;
    if (cur == stop) {
        // when the stop is the current sequence node, generate its lhs (the prefix) and return
        // but if that lhs already contains an increment pattern (i.e., the node itself is the inc-sequence), do not generate it as prefix
        if (cur->kind == ND_SEQ) { if (!node_contains_inc_pattern(cur->lhs)) gen(cur->lhs, out); return; }
        return;
    }
    if (cur->kind == ND_SEQ) {
        if (node_contains(cur->lhs, stop)) { gen_prefix_until(cur->lhs, stop, out); return; }
        // stop not in lhs -> generate lhs in full then recurse into rhs
        gen(cur->lhs, out);
        gen_prefix_until(cur->rhs, stop, out);
        return;
    }
    gen(cur, out);
}
static Node *find_tail(Node *n) {
    // treat the node itself as the tail subtree root (do not descend into its rhs)
    return n;
}
static void gen_body_label_before_last(Node *n, char *label, FILE *out) {
    if (!n) return;
    Node *tail = find_tail(n);
    if (!tail) return;
    // generate everything before the tail subtree (so we don't emit the trailing 'orig' parts of a post-inc)
    gen_prefix_until(n, tail, out);
    fprintf(out, "%s:\n", label);
    // emit only the increment assignment within the tail (search for an ND_ASSIGN inside tail)
    Node *inc = NULL;
    // search for an increment-assign pattern (i = i + 1) preferentially
    {
        Node *stack[128]; int sp = 0; if (tail) stack[sp++] = tail;
        while (sp > 0) {
            Node *cur = stack[--sp];
            if (!cur) continue;
            if (cur->kind == ND_ASSIGN && cur->lhs && cur->lhs->kind == ND_VAR && cur->rhs && cur->rhs->kind == ND_ADD) {
                Node *add = cur->rhs;
                Node *a = add->lhs, *b = add->rhs;
                if (a && b && ((a->kind == ND_VAR && cur->lhs->kind == ND_VAR && strcmp(a->name, cur->lhs->name) == 0 && b->kind == ND_NUM && b->val == 1) || (b->kind == ND_VAR && cur->lhs->kind == ND_VAR && strcmp(b->name, cur->lhs->name) == 0 && a->kind == ND_NUM && a->val == 1))) { inc = cur; break; }
            }
            if (cur->kind == ND_SEQ) { if (cur->rhs) stack[sp++] = cur->rhs; if (cur->lhs) stack[sp++] = cur->lhs; }
        }
    }
    // fallback: any assignment in tail
    if (!inc) {
        Node *stack[128]; int sp = 0; if (tail) stack[sp++] = tail;
        while (sp > 0) {
            Node *cur = stack[--sp];
            if (!cur) continue;
            if (cur->kind == ND_ASSIGN) { inc = cur; break; }
            if (cur->kind == ND_SEQ) { if (cur->rhs) stack[sp++] = cur->rhs; if (cur->lhs) stack[sp++] = cur->lhs; }
        }
    }
    if (inc) gen(inc, out);
    else {
        Node *first = find_first_of_tail(n);
        if (first) gen(first, out);
    }
}

// walk AST to collect local variable declarations
static void collect_locals(Node *node) {
    if (!node) return;
    switch (node->kind) {
    case ND_SEQ:
        collect_locals(node->lhs);
        collect_locals(node->rhs);
        return;
    case ND_DECL:
        // skip enumerator-style constants (no storage required)
        if (!node->is_const) add_lvar(node->name, node->ty);
        if (node->lhs) collect_locals(node->lhs);
        return;
    case ND_LABEL:
        // labels can precede any stmt; traverse the labeled statement to find local decls
        collect_locals(node->lhs);
        return;
    case ND_IF:
        collect_locals(node->lhs);
        collect_locals(node->rhs);
        collect_locals(node->els);
        return;
    case ND_WHILE:
    case ND_DO:
        collect_locals(node->lhs);
        collect_locals(node->rhs);
        return;
    case ND_ASSIGN:
    case ND_ADD:
    case ND_SUB:
    case ND_MUL:
    case ND_DIV:
    case ND_MOD:
    case ND_BITAND:
    case ND_BITOR:
    case ND_BITXOR:
    case ND_SHL:
    case ND_SHR:
        collect_locals(node->lhs);
        collect_locals(node->rhs);
        return;
    case ND_RETURN:
        collect_locals(node->lhs);
        return;
    default:
        return;
    }
}

// generate code for expression/statement
static void gen(Node *node, FILE *out);

static void gen_binary(Node *node, FILE *out) {
    // evaluate lhs then rhs, combine using stack
    gen(node->lhs, out);
    fprintf(out, "\tpushq %%rax\n");
    gen(node->rhs, out);
    fprintf(out, "\tpopq %%rcx\n");
    // decide whether operation is pointer-aware
    Type *lt = node->lhs ? node->lhs->ty : NULL;
    Type *rt = node->rhs ? node->rhs->ty : NULL;
    int is_ptr_op = (node->kind == ND_ADD || node->kind == ND_SUB) && ((lt && (lt->kind == TY_PTR || lt->kind == TY_ARRAY)) || (rt && (rt->kind == TY_PTR || rt->kind == TY_ARRAY)));

    if (is_ptr_op) {
        // rcx = lhs (ptr), rax = rhs (maybe int or ptr)
        if (node->kind == ND_ADD) {
            // ptr + int or int + ptr
            Type *ptrty = lt && (lt->kind == TY_PTR || lt->kind == TY_ARRAY) ? lt : rt;
            Type *base = ptrty->base;
            size_t scale = base ? size_of(base) : 1;
            // ensure rax is extended
            fprintf(out, "\tmovslq %%eax, %%rax\n");
            if (scale != 1) fprintf(out, "\timulq $%zu, %%rax\n", scale);
            // rcx is pointer; add
            fprintf(out, "\taddq %%rax, %%rcx\n");
            fprintf(out, "\tmovq %%rcx, %%rax\n");
            return;
        } else if (node->kind == ND_SUB) {
            // ptr - int  or ptr - ptr
            if ((lt && (lt->kind == TY_PTR || lt->kind == TY_ARRAY)) && (rt && (rt->kind == TY_PTR || rt->kind == TY_ARRAY))) {
                // (lhs - rhs) / size
                Type *base = lt->base;
                size_t scale = base ? size_of(base) : 1;
                // rcx = lhs, rax = rhs
                fprintf(out, "\tsubq %%rax, %%rcx\n");
                // rcx now bytes difference; divide by scale
                fprintf(out, "\tmovq %%rcx, %%rax\n");
                fprintf(out, "\tmovq $%zu, %%rsi\n", scale);
                fprintf(out, "\tcqo\n");
                fprintf(out, "\tidivq %%rsi\n");
                return;
            } else if ((lt && (lt->kind == TY_PTR || lt->kind == TY_ARRAY)) && rt && rt->kind == TY_INT) {
                // ptr - int -> ptr
                Type *base = lt->base;
                size_t scale = base ? size_of(base) : 1;
                fprintf(out, "\tmovslq %%eax, %%rax\n");
                if (scale != 1) fprintf(out, "\timulq $%zu, %%rax\n", scale);
                fprintf(out, "\tsubq %%rax, %%rcx\n");
                fprintf(out, "\tmovq %%rcx, %%rax\n");
                return;
            }
        }
    }

    // integer or comparison ops (choose 64-bit ops when operand is long or pointer)
    int use64 = 0;
    if ((lt && (lt->kind == TY_PTR || lt->kind == TY_LONG || lt->kind == TY_LONGLONG)) || (rt && (rt->kind == TY_PTR || rt->kind == TY_LONG || rt->kind == TY_LONGLONG))) use64 = 1;
    switch (node->kind) {
    case ND_ADD:
        if (use64) {
            fprintf(out, "\taddq %%rax, %%rcx\n");
            fprintf(out, "\tmovq %%rcx, %%rax\n");
        } else {
            fprintf(out, "\taddl %%ecx, %%eax\n");
        }
        break;
    case ND_SUB:
        if (use64) {
            fprintf(out, "\tsubq %%rax, %%rcx\n");
            fprintf(out, "\tmovq %%rcx, %%rax\n");
        } else {
            fprintf(out, "\tsubl %%eax, %%ecx\n");
            fprintf(out, "\tmovl %%ecx, %%eax\n");
        }
        break;
    case ND_MUL:
        if (use64) fprintf(out, "\timulq %%rcx, %%rax\n"); else fprintf(out, "\timull %%ecx, %%eax\n");
        break;
    case ND_DIV:
    case ND_MOD: {
        int uns = (lt && cg_is_unsigned(lt)) || (rt && cg_is_unsigned(rt));
        if (use64) {
            // rcx = lhs, rax = rhs? (note: earlier code uses rcx=lhs,rax=rhs then swapped)
            fprintf(out, "\tmovq %%rax, %%rbx\n");
            fprintf(out, "\tmovq %%rcx, %%rax\n");
            if (uns) {
                // unsigned division: zero-extend into rdx
                fprintf(out, "\txorq %%rdx, %%rdx\n");
                fprintf(out, "\tdivq %%rbx\n");
            } else {
                // signed division
                fprintf(out, "\tcqo\n");
                fprintf(out, "\tidivq %%rbx\n");
            }
            if (node->kind == ND_MOD) {
                // result is remainder in rdx for both signed/unsigned
                fprintf(out, "\tmovq %%rdx, %%rax\n");
            }
        } else {
            fprintf(out, "\tmovl %%eax, %%ebx\n");
            fprintf(out, "\tmovl %%ecx, %%eax\n");
            if (uns) {
                fprintf(out, "\txorl %%edx, %%edx\n");
                fprintf(out, "\tdivl %%ebx\n");
            } else {
                fprintf(out, "\tcltd\n");
                fprintf(out, "\tidivl %%ebx\n");
            }
            if (node->kind == ND_MOD) {
                fprintf(out, "\tmovl %%edx, %%eax\n");
            }
        }
        break; }
    case ND_EQ: case ND_NE: case ND_LT: case ND_LE: {
        int uns = (lt && cg_is_unsigned(lt)) || (rt && cg_is_unsigned(rt));
        if (use64) fprintf(out, "\tcmpq %%rax, %%rcx\n"); else fprintf(out, "\tcmpl %%eax, %%ecx\n");
        if (node->kind == ND_EQ) fprintf(out, "\tsete %%al\n");
        else if (node->kind == ND_NE) fprintf(out, "\tsetne %%al\n");
        else if (node->kind == ND_LT) fprintf(out, "\t%s %%al\n", uns ? "setb" : "setl");
        else if (node->kind == ND_LE) fprintf(out, "\t%s %%al\n", uns ? "setbe" : "setle");
        fprintf(out, "\tmovzbq %%al, %%rax\n");
        break; }
    case ND_BITAND:
        if (use64) {
            fprintf(out, "\tandq %%rax, %%rcx\n");
            fprintf(out, "\tmovq %%rcx, %%rax\n");
        } else {
            fprintf(out, "\tandl %%eax, %%ecx\n");
            fprintf(out, "\tmovl %%ecx, %%eax\n");
        }
        break;
    case ND_BITOR:
        if (use64) {
            fprintf(out, "\torq %%rax, %%rcx\n");
            fprintf(out, "\tmovq %%rcx, %%rax\n");
        } else {
            fprintf(out, "\torl %%eax, %%ecx\n");
            fprintf(out, "\tmovl %%ecx, %%eax\n");
        }
        break;
    case ND_BITXOR:
        if (use64) {
            fprintf(out, "\txorq %%rax, %%rcx\n");
            fprintf(out, "\tmovq %%rcx, %%rax\n");
        } else {
            fprintf(out, "\txorl %%eax, %%ecx\n");
            fprintf(out, "\tmovl %%ecx, %%eax\n");
        }
        break;
    case ND_SHL:
        if (use64) {
            fprintf(out, "\tmovb %%al, %%cl\n");
            fprintf(out, "\tshlq %%cl, %%rcx\n");
            fprintf(out, "\tmovq %%rcx, %%rax\n");
        } else {
            fprintf(out, "\tmovb %%al, %%cl\n");
            fprintf(out, "\tsall %%cl, %%ecx\n");
            fprintf(out, "\tmovl %%ecx, %%eax\n");
        }
        break;
    case ND_SHR:
        if (use64) {
            int uns = (lt && cg_is_unsigned(lt));
            fprintf(out, "\tmovb %%al, %%cl\n");
            if (uns) fprintf(out, "\tshrq %%cl, %%rcx\n"); else fprintf(out, "\tsarq %%cl, %%rcx\n");
            fprintf(out, "\tmovq %%rcx, %%rax\n");
        } else {
            int uns = (lt && cg_is_unsigned(lt));
            fprintf(out, "\tmovb %%al, %%cl\n");
            if (uns) fprintf(out, "\tshrl %%cl, %%ecx\n"); else fprintf(out, "\tsarl %%cl, %%ecx\n");
            fprintf(out, "\tmovl %%ecx, %%eax\n");
        }
        break;
    default:
        break;
    }
}

static void gen(Node *node, FILE *out) {
    if (!node) return;
    if (returned && node->kind != ND_LABEL && node->kind != ND_GOTO) return;
    switch (node->kind) {
    case ND_NUM: {
        Type *nty = node->ty ? node->ty : ty_int;
        if (size_of(nty) == 8) fprintf(out, "\tmovq $%lld, %%rax\n", node->val);
        else fprintf(out, "\tmovl $%d, %%eax\n", (int)node->val);
        return; }
    case ND_VAR: {
        LVar *v = find_lvar(node->name);
        if (v) {
            if (v->ty && v->ty->kind == TY_ARRAY) {
                // array decays to address of its first element (address-of on stack slot)
                fprintf(out, "\tleaq -%d(%%rbp), %%rax\n", v->offset);
                return;
            }
            load_from_stack_offset(v->offset, v->ty, out);
            return;
        }
        GVar *g = gvar_find(node->name);
        if (g) {
            if (g->ty && g->ty->kind == TY_ARRAY) {
                fprintf(out, "\tleaq %s(%%rip), %%rax\n", node->name);
                return;
            }
            load_global_into_rax(node->name, g->ty, out);
            return;
        }
        fprintf(stderr, "unknown variable: %s\n", node->name); exit(1);
    }
    case ND_ASSIGN: {
        // lhs can be ND_VAR or ND_DEREF
        if (node->lhs->kind == ND_VAR) {
            LVar *v = find_lvar(node->lhs->name);
            if (v) {
                gen(node->rhs, out); // result in eax/rax
                // normalize if assigning to _Bool: turn any non-zero into 1
                if (v->ty && v->ty->kind == TY_BOOL) {
                    fprintf(out, "\ttest %%rax, %%rax\n");
                    fprintf(out, "\tsetne %%al\n");
                    fprintf(out, "\tmovzbq %%al, %%rax\n");
                }
                store_to_stack_offset(v->offset, v->ty, out);
                return;
            }
            GVar *g = gvar_find(node->lhs->name);
            if (g) {
                gen(node->rhs, out);
                if (g->ty && g->ty->kind == TY_BOOL) {
                    fprintf(out, "\ttest %%rax, %%rax\n");
                    fprintf(out, "\tsetne %%al\n");
                    fprintf(out, "\tmovzbq %%al, %%rax\n");
                }
                store_rax_to_global(node->lhs->name, g->ty, out);
                return;
            }
            fprintf(stderr, "unknown variable in assignment: %s\n", node->lhs->name); exit(1);
        } else if (node->lhs->kind == ND_DEREF) {
            // evaluate pointer address into rax, save it
            gen(node->lhs->lhs, out);
            fprintf(out, "\tpushq %%rax\n");
            // evaluate rhs into rax/eax
            gen(node->rhs, out);
            // normalize for _Bool target
            Type *target = node->lhs ? node->lhs->ty : NULL; // deref's type
            fprintf(out, "\tpopq %%rcx\n");
            if (target && target->kind == TY_BOOL) {
                fprintf(out, "\ttest %%rax, %%rax\n");
                fprintf(out, "\tsetne %%al\n");
                fprintf(out, "\tmovzbq %%al, %%rax\n");
            }
            store_to_address_in_rcx(target, out);
            return;
        } else {
            fprintf(stderr, "assignment to non-variable\n"); exit(1);
        }
    }
    case ND_DECL: {
        // enum-like constants have is_const set and require no storage
        if (node->is_const) return;
        // if initializer present, evaluate and store; otherwise zero (scalars) or nothing (arrays)
        LVar *v = find_lvar(node->name);
        if (!v) { fprintf(stderr, "internal error: decl not found %s\n", node->name); exit(1); }
        if (node->lhs) {
            // array initializer for local arrays
            if (v->ty && v->ty->kind == TY_ARRAY) {
                Type *elt = v->ty->base;
                // string literal initializer for char arrays
                if (node->lhs->kind == ND_STR && elt && elt->kind == TY_CHAR) {
                    char *s = node->lhs->str;
                    int i = 0;
                    fprintf(out, "\tmovq $0, %%rax\n"); // ensure rax exists
                    for (; s && s[i] && i < (int)v->ty->len; i++) {
                        unsigned char c = s[i];
                        fprintf(out, "\tmovb $%u, -%d(%%rbp)\n", c, v->offset - i);
                    }
                    // zero remaining
                    for (; i < (int)v->ty->len; i++) fprintf(out, "\tmovb $0, -%d(%%rbp)\n", v->offset - i);
                    return;
                }
                if (node->lhs->kind == ND_INIT) {
                    // load base address into rsi
                    fprintf(out, "\tleaq -%d(%%rbp), %%rsi\n", v->offset);
                        int idx = 0;
                    for (Node *e = node->lhs->lhs; e; e = e->rhs) {
                        gen(e, out); // result in rax
                        // move base into rcx and add offset
                        fprintf(out, "\tmovq %%rsi, %%rcx\n");
                        int off = idx * (int)size_of(elt);
                        if (off) fprintf(out, "\taddq $%d, %%rcx\n", off);
                        // store rax into (rcx) according to element size
                        store_to_address_in_rcx(elt, out);
                        idx++;
                    }
                    // zero remaining elements
                    for (; idx < (int)v->ty->len; idx++) {
                        fprintf(out, "\tmovq $0, %%rax\n");
                        fprintf(out, "\tmovq %%rsi, %%rcx\n");
                        int off = idx * (int)size_of(elt);
                        if (off) fprintf(out, "\taddq $%d, %%rcx\n", off);
                        store_to_address_in_rcx(elt, out);
                    }
                    return;
                }
                // unsupported initializer for arrays -> error
                fprintf(stderr, "codegen: unsupported initializer for array %s\n", node->name); exit(1);
            }
            if (v->ty && v->ty->kind == TY_BOOL) {
                // normalize initializer value (scalar)
                gen(node->lhs, out);
                fprintf(out, "\ttest %%rax, %%rax\n");
                fprintf(out, "\tsetne %%al\n");
                fprintf(out, "\tmovzbq %%al, %%rax\n");
                store_to_stack_offset(v->offset, v->ty, out);
                return;
            }
            // scalar initializer
            gen(node->lhs, out);
            store_to_stack_offset(v->offset, v->ty, out);
            return;
        }
    }
    return;
    case ND_LABEL: {
        char *asmname = label_get_asmname(node->name);
        fprintf(out, "%s:\n", asmname);
        if (node->lhs) {
            int prev_returned = returned; returned = 0; // ensure label body always emitted
            gen(node->lhs, out);
            returned = prev_returned || returned;
        }
        return; }
    case ND_GOTO: {
        char *asmname = label_get_asmname(node->name);
        fprintf(out, "\tjmp %s\n", asmname);
        return; }
    case ND_SEQ:
        gen(node->lhs, out);
        if (returned) {
            // even if earlier code returned, we must still emit labels in the right-hand side
            Node *stack[256]; int sp = 0; if (node->rhs) stack[sp++] = node->rhs;
            while (sp > 0) {
                Node *cur = stack[--sp]; if (!cur) continue;
                if (cur->kind == ND_LABEL) { gen(cur, out); continue; }
                if (cur->kind == ND_SEQ) { if (cur->rhs) stack[sp++] = cur->rhs; if (cur->lhs) stack[sp++] = cur->lhs; }
                else { if (cur->rhs) stack[sp++] = cur->rhs; if (cur->lhs) stack[sp++] = cur->lhs; if (cur->body) stack[sp++] = cur->body; if (cur->els) stack[sp++] = cur->els; }
            }
            return;
        }
        gen(node->rhs, out);
        return;
    case ND_RETURN:
        gen(node->lhs, out);
        // epilogue
        fprintf(out, "\tleave\n\tret\n");
        returned = 1;
        return;
    case ND_ADD:
    case ND_SUB:
    case ND_MUL:
    case ND_DIV:
    case ND_MOD:
    case ND_BITAND:
    case ND_BITOR:
    case ND_BITXOR:
    case ND_SHL:
    case ND_SHR:
        gen_binary(node, out);
        return;
    case ND_EQ:
    case ND_NE:
    case ND_LT:
    case ND_LE:
        gen_binary(node, out);
        return;
    case ND_IF: {
        int cur = labelseq++;
        // condition
        gen(node->lhs, out);
        // if condition == 0 jump to else or end
        fprintf(out, "\ttest %%rax, %%rax\n");
        if (node->els) {
            fprintf(out, "\tje .Lelse%d\n", cur);
            gen(node->rhs, out);
            fprintf(out, "\tjmp .Lend%d\n", cur);
            fprintf(out, ".Lelse%d:\n", cur);
            gen(node->els, out);
            fprintf(out, ".Lend%d:\n", cur);
        } else {
            fprintf(out, "\tje .Lend%d\n", cur);
            gen(node->rhs, out);
            fprintf(out, ".Lend%d:\n", cur);
        }
        return;
    }
    case ND_SWITCH: {
        // evaluate switch expression and leave on stack for comparisons
        gen(node->lhs, out);
        fprintf(out, "\tpushq %%rax\n");

        // collect case/default nodes and assign labels
        int case_count = 0;
        int default_exists = 0;
        // first pass: count
        switch_collect_counts(node->rhs, &case_count, &default_exists);

        // second pass: assign labels
        Node **cases = NULL;
        if (case_count > 0) cases = calloc(case_count, sizeof(Node*));
        int ci = 0;
        char *default_label = NULL;
        switch_collect_fill(node->rhs, cases, &ci, &default_label);

        // emit comparisons and jumps to matching case labels
        fprintf(out, "\tmovq (%%rsp), %%rcx\n");
        for (int i = 0; i < case_count; i++) {
            long val;
            if (!eval_const_int(cases[i]->lhs, &val)) { fprintf(stderr, "codegen: case value must be constant (line %d)\n", cases[i]->line); exit(1); }
            fprintf(out, "\tcmpq $%ld, %%rcx\n", val);
            fprintf(out, "\tje %s\n", cases[i]->glabel);
        }
        if (default_label) fprintf(out, "\tjmp %s\n", default_label); else {
            int endlab = labelseq++;
            fprintf(out, "\tjmp .Lend%d\n", endlab);
            // we'll need to emit .Lend... after body
            // push end label onto break stack
            break_label_stack[break_sp++] = endlab;

            // emit case labels and bodies
            switch_gen_body(node->rhs, out);

            // pop switch save and emit end
            fprintf(out, "\taddq $8, %%rsp\n");
            fprintf(out, ".Lend%d:\n", endlab);
            // pop break stack
            break_sp--;
        }

        // if default exists or not, we still need to emit case bodies and labels in proper order
        if (default_label) {
            // push an end label onto break stack
            int endlab = labelseq++;
            break_label_stack[break_sp++] = endlab;
            // emit labels and bodies in order
            switch_gen_body(node->rhs, out);
            // cleanup
            fprintf(out, "\taddq $8, %%rsp\n");
            fprintf(out, ".Lend%d:\n", endlab);
            break_sp--;
        }
        return;
    }
    case ND_WHILE: {
        int cur = labelseq++;
        // determine if this loop has an "inc" at its tail (for transformed for-loops)
        int has_inc = 0;
        if (node->rhs && node->rhs->kind == ND_SEQ && node->rhs->rhs) has_inc = 1;

        // setup continue target (either a dedicated label before the trailing inc, or the loop begin)
        char contbuf[32];
        if (has_inc) {
            int cont_id = labelseq++;
            sprintf(contbuf, ".Lcont%d", cont_id);
            continue_label_stack[continue_sp++] = strdup(contbuf);
        } else {
            sprintf(contbuf, ".Lbegin%d", cur);
            continue_label_stack[continue_sp++] = strdup(contbuf);
        }

        // push break target
        break_label_stack[break_sp++] = cur;

        fprintf(out, ".Lbegin%d:\n", cur);
        gen(node->lhs, out); // condition
        fprintf(out, "\ttest %%rax, %%rax\n");
        fprintf(out, "\tje .Lend%d\n", cur);

        if (has_inc) {
            // emit body with continue label placed before the last statement (the inc)
            gen_body_label_before_last(node->rhs, continue_label_stack[continue_sp - 1], out);
        } else {
            gen(node->rhs, out); // body
        }

        fprintf(out, "\tjmp .Lbegin%d\n", cur);
        fprintf(out, ".Lend%d:\n", cur);

        // pop break/continue targets
        break_sp--;
        free(continue_label_stack[--continue_sp]);
        return;
    }
    case ND_DO: {
        int cur = labelseq++;
        // continue should jump to condition evaluation
        char contbuf[32];
        sprintf(contbuf, ".Lcond%d", cur);
        continue_label_stack[continue_sp++] = strdup(contbuf);

        // push break target
        break_label_stack[break_sp++] = cur;

        fprintf(out, ".Lbegin%d:\n", cur);
        gen(node->rhs, out); // body
        fprintf(out, "%s:\n", contbuf);
        gen(node->lhs, out); // condition
        fprintf(out, "\ttest %%rax, %%rax\n");
        fprintf(out, "\tjne .Lbegin%d\n", cur);
        fprintf(out, ".Lend%d:\n", cur);

        // pop break/continue
        break_sp--;
        free(continue_label_stack[--continue_sp]);
        return;
    }
    case ND_LAND: {
        int cur = labelseq++;
        // evaluate lhs
        gen(node->lhs, out);
        // if lhs == 0 -> false
        fprintf(out, "\ttest %%rax, %%rax\n");
        fprintf(out, "\tje .Lland_false%d\n", cur);
        // lhs true -> evaluate rhs
        gen(node->rhs, out);
        // convert rhs to 0/1
        fprintf(out, "\ttest %%rax, %%rax\n");
        fprintf(out, "\tsetne %%al\n");
        fprintf(out, "\tmovzbq %%al, %%rax\n");
        fprintf(out, "\tjmp .Lland_end%d\n", cur);
        fprintf(out, ".Lland_false%d:\n", cur);
        fprintf(out, "\tmovl $0, %%eax\n");
        fprintf(out, ".Lland_end%d:\n", cur);
        return;
    }
    case ND_LOR: {
        int cur = labelseq++;
        // evaluate lhs
        gen(node->lhs, out);
        // if lhs != 0 -> true
        fprintf(out, "\ttest %%rax, %%rax\n");
        fprintf(out, "\tjne .Llor_true%d\n", cur);
        // lhs false -> evaluate rhs
        gen(node->rhs, out);
        // convert rhs to 0/1
        fprintf(out, "\ttest %%rax, %%rax\n");
        fprintf(out, "\tsetne %%al\n");
        fprintf(out, "\tmovzbq %%al, %%rax\n");
        fprintf(out, "\tjmp .Llor_end%d\n", cur);
        fprintf(out, ".Llor_true%d:\n", cur);
        fprintf(out, "\tmovl $1, %%eax\n");
        fprintf(out, ".Llor_end%d:\n", cur);
        return;
    }
    case ND_STR: {
        char *label = emit_string_literal(out, node->str);
        fprintf(out, "\tleaq %s(%%rip), %%rax\n", label);
        return;
    }
    case ND_CASE:
    case ND_DEFAULT:
        // label-only nodes (handled by switch emission)
        return;
    case ND_BREAK: {
        if (break_sp == 0) { fprintf(stderr, "codegen: 'break' used outside switch/loop\n"); exit(1); }
        int target = break_label_stack[break_sp - 1];
        fprintf(out, "\tjmp .Lend%d\n", target);
        return;
    }
    case ND_CONTINUE: {
        if (continue_sp == 0) { fprintf(stderr, "codegen: 'continue' used outside loop\n"); exit(1); }
        fprintf(out, "\tjmp %s\n", continue_label_stack[continue_sp - 1]);
        return;
    }
    case ND_CALL: {
        // collect args into an array
        int argc = 0;
        for (Node *a = node->lhs; a; a = a->rhs) argc++;

        // allocate temporary space to store all args; we'll store 8 bytes per arg
        int temp_bytes = argc * 8;
        // ensure 16-byte alignment for the allocated temp area + pushed extras
        int extra = argc > 6 ? (argc - 6) : 0;
        int total_tmp = temp_bytes + extra * 8;
        int aligned = ((total_tmp + 15) / 16) * 16;
        if (aligned > 0) fprintf(out, "\tsubq $%d, %%rsp\n", aligned);

        // store args left-to-right into temp area at (%rsp + i*8)
        int i = 0;
        for (Node *a = node->lhs; a; a = a->rhs) {
            gen(a, out); // result in eax/rax
            Type *at = a->ty ? a->ty : ty_int;
            int asz = (int)size_of(at);
            if (asz == 8) fprintf(out, "\tmovq %%rax, %d(%%rsp)\n", i * 8);
            else /* 4/2/1 */ fprintf(out, "\tmovl %%eax, %d(%%rsp)\n", i * 8);
            i++;
        }

        const char *argregs32[] = {"%edi", "%esi", "%edx", "%ecx", "%r8d", "%r9d"};
        const char *argregs64[] = {"%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9"};
        // move first up-to-6 args into registers
        int nreg = argc < 6 ? argc : 6;
        for (i = 0; i < nreg; i++) {
            int off = i * 8;
            Node *a = node->lhs;
            for (int j = 0; j < i; j++) a = a->rhs;
            Type *at = a->ty ? a->ty : ty_int;
            int asz = (int)size_of(at);
            if (asz == 8) fprintf(out, "\tmovq %d(%%rsp), %s\n", off, argregs64[i]);
            else fprintf(out, "\tmovl %d(%%rsp), %s\n", off, argregs32[i]);
        }

        // push extra args (arg argc-1 down to arg 6) in reverse order so arg6 is closest to return address
        for (i = argc - 1; i >= 6; i--) {
            int off = i * 8;
            Node *a = node->lhs;
            for (int j = 0; j < i; j++) a = a->rhs;
            Type *at = a->ty ? a->ty : ty_int;
            int asz = (int)size_of(at);
            if (asz == 8) {
                fprintf(out, "\tmovq %d(%%rsp), %%rax\n", off);
                fprintf(out, "\tpushq %%rax\n");
            } else {
                fprintf(out, "\tmovl %d(%%rsp), %%eax\n", off);
                fprintf(out, "\tpushq %%rax\n");
            }
        }

        // call
        fprintf(out, "\tcall %s\n", node->name);

        // cleanup pushed extras
        if (extra > 0) fprintf(out, "\taddq $%d, %%rsp\n", extra * 8);

        // restore stack pointer to remove temp area
        if (aligned > 0) fprintf(out, "\taddq $%d, %%rsp\n", aligned);
        return;
    }
    case ND_DEREF: {
        // evaluate pointer into rax
        gen(node->lhs, out);
        if (!node->ty) { fprintf(stderr, "codegen: missing type for deref\n"); exit(1); }
        load_from_address_in_rax(node->ty, out);
        return;
    }
    case ND_ADDR: {
        // only allow address of variable
        if (node->lhs->kind != ND_VAR) { fprintf(stderr, "codegen: address-of expects variable\n"); exit(1); }
        LVar *v = find_lvar(node->lhs->name);
        if (v) { fprintf(out, "\tleaq -%d(%%rbp), %%rax\n", v->offset); return; }
        GVar *g = gvar_find(node->lhs->name);
        if (g) { fprintf(out, "\tleaq %s(%%rip), %%rax\n", node->lhs->name); return; }
        fprintf(stderr, "unknown variable: %s\n", node->lhs->name); exit(1);
    }
    case ND_NOT: {
        // evaluate operand into rax
        gen(node->lhs, out);
        // set rax = (operand == 0) ? 1 : 0
        fprintf(out, "\ttest %%rax, %%rax\n");
        fprintf(out, "\tsete %%al\n");
        fprintf(out, "\tmovzbq %%al, %%rax\n");
        return;
    }
    case ND_BITNOT: {
        gen(node->lhs, out);
        if (node->lhs && node->lhs->ty && size_of(node->lhs->ty) == 8) fprintf(out, "\tnotq %%rax\n");
        else fprintf(out, "\tnotl %%eax\n");
        return;
    }
    default:
        fprintf(stderr, "codegen: unsupported node kind %d\n", node->kind);
        exit(1);
    }
}

static char *emit_string_literal(FILE *out, char *s) {
    int id = strseq++;
    char *label = malloc(32);
    sprintf(label, ".LSTR%d", id);
    fprintf(out, "\t.data\n%s:\n\t.asciz \"", label);
    for (char *p = s; p && *p; p++) {
        unsigned char c = *p;
        if (c == '\\') fprintf(out, "\\\\");
        else if (c == '"') fprintf(out, "\\\"");
        else if (c == '\n') fprintf(out, "\\n");
        else if (c == '\t') fprintf(out, "\\t");
        else if (c >= 32 && c < 127) fputc(c, out);
        else fprintf(out, "\\x%02x", c);
    }
    fprintf(out, "\"\n\t.text\n");
    return label;
}

static int eval_const_int(Node *n, long *out) {
    if (!n) return 0;
    long a, b;
    switch (n->kind) {
    case ND_NUM: *out = n->val; fprintf(stderr, "[DBG eval_const] NUM %ld\n", *out); return 1;
    case ND_ADD: if (eval_const_int(n->lhs, &a) && eval_const_int(n->rhs, &b)) { *out = a + b; fprintf(stderr, "[DBG eval_const] ADD %ld\n", *out); return 1; } return 0;
    case ND_SUB: fprintf(stderr, "[DBG eval_const] SUB lhs=%p rhs=%p\n", (void*)n->lhs, (void*)n->rhs);
        if ((n->lhs == NULL && eval_const_int(n->rhs, &b))) { *out = -b; fprintf(stderr, "[DBG eval_const] UNARY_MINUS %ld\n", *out); return 1; }
        if (eval_const_int(n->lhs, &a) && eval_const_int(n->rhs, &b)) { *out = a - b; fprintf(stderr, "[DBG eval_const] SUB %ld\n", *out); return 1; }
        return 0;
    case ND_MUL: if (eval_const_int(n->lhs, &a) && eval_const_int(n->rhs, &b)) { *out = a * b; fprintf(stderr, "[DBG eval_const] MUL %ld\n", *out); return 1; } return 0;
    case ND_DIV: if (eval_const_int(n->lhs, &a) && eval_const_int(n->rhs, &b) && b != 0) { *out = a / b; fprintf(stderr, "[DBG eval_const] DIV %ld\n", *out); return 1; } return 0;
    case ND_BITAND: if (eval_const_int(n->lhs, &a) && eval_const_int(n->rhs, &b)) { *out = a & b; fprintf(stderr, "[DBG eval_const] AND %ld\n", *out); return 1; } return 0;
    case ND_BITOR: if (eval_const_int(n->lhs, &a) && eval_const_int(n->rhs, &b)) { *out = a | b; fprintf(stderr, "[DBG eval_const] OR %ld\n", *out); return 1; } return 0;
    case ND_BITXOR: if (eval_const_int(n->lhs, &a) && eval_const_int(n->rhs, &b)) { *out = a ^ b; fprintf(stderr, "[DBG eval_const] XOR %ld\n", *out); return 1; } return 0;
    case ND_SHL: if (eval_const_int(n->lhs, &a) && eval_const_int(n->rhs, &b)) { *out = a << b; fprintf(stderr, "[DBG eval_const] SHL %ld\n", *out); return 1; } return 0;
    case ND_SHR: if (eval_const_int(n->lhs, &a) && eval_const_int(n->rhs, &b)) { *out = a >> b; fprintf(stderr, "[DBG eval_const] SHR %ld\n", *out); return 1; } return 0;
    case ND_BITNOT: if (eval_const_int(n->lhs, &a)) { *out = ~a; fprintf(stderr, "[DBG eval_const] NOT %ld\n", *out); return 1; } return 0;
    default: return 0;
    }
}

static void emit_globals(Node *top, FILE *out) {
    for (Node *n = top; n; n = n->rhs) {
        if (n->kind != ND_DECL || !n->is_global) continue;
        Type *ty = n->ty ? n->ty : ty_int;
        // register global in symbol table; if already registered skip emission
        int nondef_extern = (n->is_extern && !n->lhs);
        if (!gvar_push(n->name, ty, n->is_static, nondef_extern)) continue;
        int size = (int)size_of(ty);
        // alignment: use element alignment for arrays, otherwise type size (must be power of two)
        int align = (ty && ty->kind == TY_ARRAY && ty->base) ? (int)size_of(ty->base) : size;
        if (!n->lhs) {
            // uninitialized global: reserve via .comm
            fprintf(out, "\t.comm %s, %d, %d\n", n->name, size, align);
            continue;
        }
        // initializer present
        long val;
        if (n->lhs) fprintf(stderr, "[DBG emit_globals] %s init_kind=%d\n", n->name, n->lhs->kind);
        if (eval_const_int(n->lhs, &val)) {
            if (ty && ty->kind == TY_BOOL) val = val ? 1 : 0;
            fprintf(stderr, "[DBG emit_globals] %s const val=%ld size=%d\n", n->name, val, size);
            fprintf(out, "\t.globl %s\n\t.data\n\t.align %d\n\t.type %s, @object\n%s:\n", n->name, align, n->name, n->name);
            if (size == 8) fprintf(out, "\t.quad %ld\n", val);
            else if (size == 4) fprintf(out, "\t.long %ld\n", val);
            else if (size == 2) fprintf(out, "\t.short %ld\n", val);
            else if (size == 1) fprintf(out, "\t.byte %ld\n", val);
            else { fprintf(stderr, "unsupported global init size %d\n", size); exit(1); }
            fprintf(out, "\t.text\n");
            continue;
        }
        // array-specific initializers
        if (ty && ty->kind == TY_ARRAY) {
            Type *elt = ty->base;
            if (n->lhs->kind == ND_STR && elt && elt->kind == TY_CHAR) {
                // emit bytes for string, truncated/padded to array length
                size_t slen = strlen(n->lhs->str);
                fprintf(out, "\t.globl %s\n\t.data\n\t.align %d\n\t.type %s, @object\n%s:\n", n->name, align, n->name, n->name);
                size_t i;
                for (i = 0; i < ty->len && i < slen; i++) fprintf(out, "\t.byte %d\n", (unsigned char)n->lhs->str[i]);
                for (; i < ty->len; i++) fprintf(out, "\t.byte 0\n");
                fprintf(out, "\t.text\n");
                continue;
            }
            if (n->lhs->kind == ND_INIT) {
                fprintf(out, "\t.globl %s\n\t.data\n\t.align %d\n\t.type %s, @object\n%s:\n", n->name, align, n->name, n->name);
                int i = 0;
                for (Node *e = n->lhs->lhs; e; e = e->rhs) {
                    long v;
                    if (!eval_const_int(e, &v)) { fprintf(stderr, "global array init must be constant (line %d)\n", e->line); exit(1); }
                    if (elt && size_of(elt) == 8) fprintf(out, "\t.quad %ld\n", v);
                    else if (elt && size_of(elt) == 4) fprintf(out, "\t.long %ld\n", v);
                    else if (elt && size_of(elt) == 2) fprintf(out, "\t.short %ld\n", v);
                    else if (elt && size_of(elt) == 1) fprintf(out, "\t.byte %ld\n", v);
                    else { fprintf(stderr, "unsupported array element size\n"); exit(1); }
                    i++;
                }
                for (; i < (int)ty->len; i++) {
                    if (elt && size_of(elt) == 8) fprintf(out, "\t.quad 0\n");
                    else if (elt && size_of(elt) == 4) fprintf(out, "\t.long 0\n");
                    else if (elt && size_of(elt) == 2) fprintf(out, "\t.short 0\n");
                    else if (elt && size_of(elt) == 1) fprintf(out, "\t.byte 0\n");
                }
                fprintf(out, "\t.text\n");
                continue;
            }
        }
        if (n->lhs->kind == ND_STR) {
            char *lbl = emit_string_literal(out, n->lhs->str);
            fprintf(out, "\t.globl %s\n\t.data\n\t.align %d\n\t.type %s, @object\n%s:\n\t.quad %s\n\t.text\n", n->name, align, n->name, n->name, lbl);
            continue;
        }
        if (n->lhs->kind == ND_ADDR && n->lhs->lhs && n->lhs->lhs->kind == ND_VAR) {
            fprintf(out, "\t.globl %s\n\t.data\n\t.align %d\n\t.type %s, @object\n%s:\n\t.quad %s\n\t.text\n", n->name, align, n->name, n->name, n->lhs->lhs->name);
            continue;
        }
        // unsupported or complex initializer: fall back to zero-initialized
        fprintf(out, "\t.comm %s, %d, %d\n", n->name, size, align);
    }
}

void codegen(Node *node, FILE *out) {
    if (!node) return;

    fprintf(stderr, "[codegen] node kind=%d name=%s is_global=%d\n", node->kind, node->name ? node->name : "?", node->is_global);

    // if a single top-level global declaration is passed, emit it
    if (node->kind == ND_DECL && node->is_global) {
        emit_globals(node, out);
        if (node->rhs) codegen(node->rhs, out);
        return;
    }

    if (node->kind == ND_SEQ) {
        // emit globals first
        emit_globals(node, out);
        // generate code for all functions in the top-level list
        for (Node *n = node; n; n = n->rhs) {
            if (n->kind == ND_FUNC) codegen(n, out);
        }
        return;
    }

    if (node->kind != ND_FUNC) {
        fprintf(stderr, "codegen: expected function node (got %d)\n", node->kind);
        exit(1);
    }

    // debug (verbose)
    if (opt_verbose) fprintf(stderr, "[codegen] node %s defined=%d\n", node->name ? node->name : "?", node->body != NULL);

    // skip forward declarations (no body) but continue to the next top-level node
    if (node->kind == ND_FUNC && !node->body) { if (node->rhs) codegen(node->rhs, out); return; }

    // collect locals from function body and params
    locals = NULL;
    stack_size = 0;
    returned = 0;

    // add params first so they occupy lowest offsets
    int param_count = 0;
    const char *argregs64[] = {"%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9"};
    for (Node *p = node->params; p; p = p->rhs) {
        add_lvar(p->name, p->ty);
        param_count++;
    }

    collect_locals(node->body);

    // align stack to 16 bytes
    int aligned = ((stack_size + 15) / 16) * 16;

    fprintf(out, "\t.text\n");
    fprintf(out, "\t.globl %s\n", node->name && node->name[0] ? node->name : "main");
    fprintf(out, "%s:\n", node->name && node->name[0] ? node->name : "main");
    fprintf(out, "\tpushq %%rbp\n");
    fprintf(out, "\tmovq %%rsp, %%rbp\n");
    if (aligned > 0) fprintf(out, "\tsubq $%d, %%rsp\n", aligned);

    // move parameter registers into their stack slots (in order)
    int idx = 0;
    idx = 0;
    for (Node *p = node->params; p; p = p->rhs) {
        if (idx >= 6) break;
        LVar *v = find_lvar(p->name);
        const char *reg = argregs64[idx];
        // emit movl from 32-bit register name (%edi, %esi, ...)
        if (v->ty && (v->ty->kind == TY_PTR || v->ty->kind == TY_LONG || v->ty->kind == TY_LONGLONG)) {
            if (strcmp(reg, "%rdi") == 0) fprintf(out, "\tmovq %%rdi, -%d(%%rbp)\n", v->offset);
            else if (strcmp(reg, "%rsi") == 0) fprintf(out, "\tmovq %%rsi, -%d(%%rbp)\n", v->offset);
            else if (strcmp(reg, "%rdx") == 0) fprintf(out, "\tmovq %%rdx, -%d(%%rbp)\n", v->offset);
            else if (strcmp(reg, "%rcx") == 0) fprintf(out, "\tmovq %%rcx, -%d(%%rbp)\n", v->offset);
            else if (strcmp(reg, "%r8") == 0) fprintf(out, "\tmovq %%r8, -%d(%%rbp)\n", v->offset);
            else if (strcmp(reg, "%r9") == 0) fprintf(out, "\tmovq %%r9, -%d(%%rbp)\n", v->offset);
        } else {
            if (strcmp(reg, "%rdi") == 0) fprintf(out, "\tmovl %%edi, -%d(%%rbp)\n", v->offset);
            else if (strcmp(reg, "%rsi") == 0) fprintf(out, "\tmovl %%esi, -%d(%%rbp)\n", v->offset);
            else if (strcmp(reg, "%rdx") == 0) fprintf(out, "\tmovl %%edx, -%d(%%rbp)\n", v->offset);
            else if (strcmp(reg, "%rcx") == 0) fprintf(out, "\tmovl %%ecx, -%d(%%rbp)\n", v->offset);
            else if (strcmp(reg, "%r8") == 0) fprintf(out, "\tmovl %%r8d, -%d(%%rbp)\n", v->offset);
            else if (strcmp(reg, "%r9") == 0) fprintf(out, "\tmovl %%r9d, -%d(%%rbp)\n", v->offset);
        }
        // normalize _Bool parameter (ensure stored value is 0 or 1)
        if (v->ty && v->ty->kind == TY_BOOL) {
            fprintf(out, "\tmovl -%d(%%rbp), %%eax\n", v->offset);
            fprintf(out, "\ttest %%eax, %%eax\n");
            fprintf(out, "\tsetne %%al\n");
            fprintf(out, "\tmovb %%al, -%d(%%rbp)\n", v->offset);
        }
        idx++;
    }

    // set current return type for normalization of _Bool returns
    cur_func_ret = node->ty;
    gen(node->body, out);
    cur_func_ret = NULL;

    // always emit a function epilogue to ensure correct control flow even if
    // branches inside the function already returned earlier. This prevents
    // jumping to a label that falls through into garbage when a branch used
    // an early return (e.g., if (cond) return x; ...).
    fprintf(out, "\tmovl $0, %%eax\n");
    fprintf(out, "\tleave\n\tret\n");
    // if this function node is part of a linked list via rhs, generate the next
    if (node->rhs) codegen(node->rhs, out);
}
