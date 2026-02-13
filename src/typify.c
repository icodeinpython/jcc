
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "../include/type.h"
#include "../include/parser.h"

// simple var symbol table (per function scope)
typedef struct Var Var;
struct Var { char *name; Type *ty; Var *next; int is_register; int is_restrict; };

// function table
typedef struct Func Func;
struct Func { char *name; Type *ret; Node *params; int defined; Func *next; };

static Func *funcs;
static Var *globals;

static void error_at_node(Node *node, const char *fmt, ...);

// helper: is an integer type (char/short/int/long and unsigned variants)
static int is_integer_type(Type *t) {
    if (!t) return 0;
    return t->kind == TY_INT || t->kind == TY_CHAR || t->kind == TY_SHORT || t->kind == TY_LONG || t->kind == TY_LONGLONG
        || t->kind == TY_UINT || t->kind == TY_UCHAR || t->kind == TY_USHORT || t->kind == TY_ULONG || t->kind == TY_ULONGLONG || t->kind == TY_BOOL;
}

static int is_unsigned_type(Type *t) {
    if (!t) return 0;
    return t->kind == TY_UINT || t->kind == TY_UCHAR || t->kind == TY_USHORT || t->kind == TY_ULONG || t->kind == TY_ULONGLONG || t->kind == TY_BOOL;
}

// choose a common integer type for binary arithmetic/operations
static Type *common_integer_type(Type *a, Type *b) {
    if (!a) return b;
    if (!b) return a;
    size_t sa = size_of(a), sb = size_of(b);
    if (sa > sb) return a;
    if (sb > sa) return b;
    // sizes equal: if either unsigned, pick unsigned counterpart
    if (is_unsigned_type(a) || is_unsigned_type(b)) {
        if (sa == 1) return ty_uchar;
        if (sa == 2) return ty_ushort;
        if (sa == 4) return ty_uint;
        return ty_ulonglong;
    }
    // both signed -> pick signed corresponding type
    if (sa == 1) return ty_char;
    if (sa == 2) return ty_short;
    if (sa == 4) return ty_int;
    return ty_longlong;
}

// compare parameter lists for equality (types)
static int params_equal(Node *a, Node *b) {
    for (;; a = a ? a->rhs : NULL, b = b ? b->rhs : NULL) {
        if (!a && !b) return 1;
        if (!a || !b) return 0;
        Type *ta = a->ty ? a->ty : ty_int;
        Type *tb = b->ty ? b->ty : ty_int;
        if (!type_equal(ta, tb)) return 0;
    }
}

// simple name list for label/goto validation
typedef struct NameList NameList;
struct NameList { char *name; NameList *next; };

static void collect_labels_and_gotos(Node *n, NameList **labs, NameList **gotos) {
    if (!n) return;
    switch (n->kind) {
    case ND_LABEL: {
        // duplicate labels are an error
        for (NameList *it = *labs; it; it = it->next) if (strcmp(it->name, n->name) == 0) error_at_node(n, "duplicate label '%s'", n->name);
        NameList *nl = calloc(1, sizeof(NameList)); nl->name = strdup(n->name); nl->next = *labs; *labs = nl;
        collect_labels_and_gotos(n->lhs, labs, gotos);
        return; }
    case ND_GOTO: {
        NameList *ng = calloc(1, sizeof(NameList)); ng->name = strdup(n->name); ng->next = *gotos; *gotos = ng; return; }
    case ND_SEQ:
        collect_labels_and_gotos(n->lhs, labs, gotos);
        collect_labels_and_gotos(n->rhs, labs, gotos);
        return;
    default:
        collect_labels_and_gotos(n->lhs, labs, gotos);
        collect_labels_and_gotos(n->rhs, labs, gotos);
        collect_labels_and_gotos(n->body, labs, gotos);
        collect_labels_and_gotos(n->els, labs, gotos);
        return;
    }
}

static void add_func_entry(char *name, Type *ret, Node *params, int defined, Node *node) {
    Func *f = NULL;
    for (Func *it = funcs; it; it = it->next) {
        if (strcmp(it->name, name) == 0) { f = it; break; }
    }
    if (!f) {
        f = calloc(1, sizeof(Func));
        f->name = strdup(name);
        f->ret = ret;
        f->params = params;
        f->defined = defined;
        f->next = funcs;
        funcs = f;
        return;
    }
    // existing declaration/definition found: check compatibility
    if (defined && f->defined) {
        error_at_node(node, "multiple definitions of function '%s'", name);
    }
    // check return type
    if (!type_equal(f->ret ? f->ret : ty_int, ret ? ret : ty_int)) {
        error_at_node(node, "conflicting return type for '%s'", name);
    }
    // check params
    if (!params_equal(f->params, params)) {
        error_at_node(node, "conflicting parameter types for '%s'", name);
    }
    if (defined) f->defined = 1;
}

static Func *find_func(char *name) {
    for (Func *f = funcs; f; f = f->next) {
        if (strcmp(f->name, name) == 0) return f;
    }
    return NULL;
}

#include <stdarg.h>

static void error_at_node(Node *node, const char *fmt, ...) {
    fprintf(stderr, "type error: ");
    va_list ap;
    va_start(ap, fmt);
    vfprintf(stderr, fmt, ap);
    va_end(ap);
    if (node) fprintf(stderr, " (line %d)", node->line);
    fprintf(stderr, "\n");
    exit(1);
}

// lookup var in current table (search locals first, then globals)
static Type *var_get(Var *vars, char *name) {
    for (Var *v = vars; v; v = v->next) {
        if (strcmp(v->name, name) == 0) return v->ty;
    }
    for (Var *v = globals; v; v = v->next) {
        if (strcmp(v->name, name) == 0) return v->ty;
    }
    return NULL;
}

static int var_is_register(Var *vars, char *name) {
    for (Var *v = vars; v; v = v->next) {
        if (strcmp(v->name, name) == 0) return v->is_register;
    }
    for (Var *v = globals; v; v = v->next) {
        if (strcmp(v->name, name) == 0) return v->is_register;
    }
    return 0;
}

static Var *var_push(Var *vars, char *name, Type *ty, int is_register, int is_restrict) {
    Var *v = calloc(1, sizeof(Var));
    v->name = strdup(name);
    v->ty = ty;
    v->is_register = is_register;
    v->is_restrict = is_restrict;
    v->next = vars;
    return v;
}

// forward
static void check(Node *node, Var **vars, Type *func_ret);
static int consts_lookup(const char *name, long *out);
static void consts_add(const char *name, long val);
static int eval_const_expr(Node *n, long *out);

static void check_binary(Node *node, Var **vars, Type *func_ret) {
    check(node->lhs, vars, func_ret);
    check(node->rhs, vars, func_ret);
    Type *lt = node->lhs ? node->lhs->ty : NULL;
    Type *rt = node->rhs ? node->rhs->ty : NULL;
    switch (node->kind) {
    case ND_ADD:
    case ND_SUB:
        // handle array decay: array + int -> pointer
        if (lt && lt->kind == TY_ARRAY && rt && rt->kind == TY_INT) { node->ty = pointer_to(lt->base); return; }
        if (rt && rt->kind == TY_ARRAY && lt && lt->kind == TY_INT) { if (node->kind == ND_SUB) error_at_node(node, "cannot do int - ptr"); node->ty = pointer_to(rt->base); return; }
        if (lt && lt->kind == TY_PTR && rt && rt->kind == TY_INT) {
            // ptr +/- int -> ptr
            node->ty = lt;
            return;
        }
        if (rt && rt->kind == TY_PTR && lt && lt->kind == TY_INT) {
            if (node->kind == ND_SUB) error_at_node(node, "cannot do int - ptr");
            node->ty = rt;
            return;
        }
        // allow arithmetic between integer types (promote to common type)
        if (lt && rt && is_integer_type(lt) && is_integer_type(rt)) {
            node->ty = common_integer_type(lt, rt);
            return;
        }
        error_at_node(node, "invalid operands to + or -");
        break;
    case ND_MUL:
    case ND_DIV:
    case ND_MOD:
        if (lt && rt && is_integer_type(lt) && is_integer_type(rt)) { node->ty = common_integer_type(lt, rt); return; }
        error_at_node(node, "invalid operands to * / or %");
        break;
    case ND_BITAND:
    case ND_BITOR:
    case ND_BITXOR:
        if (lt && rt && is_integer_type(lt) && is_integer_type(rt)) { node->ty = common_integer_type(lt, rt); return; }
        error_at_node(node, "invalid operands to bitwise operator");
        break;
    case ND_SHL:
    case ND_SHR:
        if (lt && rt && is_integer_type(lt) && is_integer_type(rt)) { node->ty = common_integer_type(lt, rt); return; }
        error_at_node(node, "invalid operands to shift");
        break;
    case ND_EQ: case ND_NE: case ND_LT: case ND_LE:
        if (lt && rt) {
            // treat arrays as pointers for comparison
            Type *lt2 = lt, *rt2 = rt;
            if (lt2->kind == TY_ARRAY) lt2 = pointer_to(lt2->base);
            if (rt2->kind == TY_ARRAY) rt2 = pointer_to(rt2->base);
            if (lt2->kind == TY_PTR || rt2->kind == TY_PTR) {
                if (!type_equal(lt2, rt2)) error_at_node(node, "pointer types mismatch in comparison");
            } else if (!(is_integer_type(lt) && is_integer_type(rt))) error_at_node(node, "invalid types in comparison");
            node->ty = ty_int; return;
        }
        error_at_node(node, "comparison with unknown types");
        break;
    default:
        error_at_node(node, "unsupported binary in check_binary");
    }
}

static void check(Node *node, Var **vars, Type *func_ret) {
    if (!node) return;
    switch (node->kind) {
    case ND_NUM:
        // literal numbers: if unspecified type, keep as int; else ty set during parsing (e.g. LL suffix)
        if (!node->ty) node->ty = ty_int;
        return;
    case ND_VAR: {
        // fold enum/const globals into literal numbers when possible
        long cv;
        if (consts_lookup(node->name, &cv)) {
            node->kind = ND_NUM; node->val = cv; node->ty = ty_int; node->name = NULL; return;
        }
        Type *t = var_get(*vars, node->name);
        if (!t) { error_at_node(node, "unknown variable: %s", node->name); }
        node->ty = t;
        return; }
    case ND_DECL: {
        // Global declarations are handled separately during top-level processing
        if (node->is_global) {
            if (node->lhs) check(node->lhs, vars, func_ret);
            return;
        }
        // lhs initializer
        if (node->lhs) {
            // array initializer lists for local arrays
            if (node->ty && node->ty->kind == TY_ARRAY && node->lhs->kind == ND_INIT) {
                int count = 0;
                for (Node *e = node->lhs->lhs; e; e = e->rhs) {
                    check(e, vars, func_ret);
                    if (!e->ty) error_at_node(node, "unknown type in array initializer");
                    if (!type_equal(e->ty, node->ty->base) && !(is_integer_type(e->ty) && is_integer_type(node->ty->base))) error_at_node(node, "array initializer type mismatch");
                    count++;
                }
                if (count > (int)node->ty->len) error_at_node(node, "too many initializers for array");
            } else {
                check(node->lhs, vars, func_ret);
            }
        }
        // if this is an enum-like constant (is_const) and has a constant initializer, add to consts table
        if (node->is_const && node->lhs) {
            long v; if (eval_const_expr(node->lhs, &v)) consts_add(node->name, v);
        }
        // add to var table (local)
        *vars = var_push(*vars, node->name, node->ty ? node->ty : ty_int, node->is_register, node->is_restrict);
        return; }
    case ND_ASSIGN: {
        check(node->lhs, vars, func_ret);
        check(node->rhs, vars, func_ret);
        Type *lt = node->lhs ? node->lhs->ty : NULL;
        Type *rt = node->rhs ? node->rhs->ty : NULL;
        if (!lt || !rt) error_at_node(node, "unknown type in assignment");
        if (type_equal(lt, rt)) { node->ty = lt; return; }
        // allow assigning between integer types (implicit conversion)
        if (is_integer_type(lt) && is_integer_type(rt)) { node->ty = lt; return; }
        // allow assigning 0 to pointer
        if (lt->kind == TY_PTR && node->rhs->kind == ND_NUM && node->rhs->val == 0) { node->ty = lt; return; }
        error_at_node(node, "type mismatch in assignment");
        return; }
    case ND_RETURN: {
        check(node->lhs, vars, func_ret);
        if (!node->lhs) {
            if (func_ret && func_ret->kind != TY_VOID) error_at_node(node, "return type mismatch");
            return;
        }
        if (!type_equal(node->lhs->ty, func_ret)) {
            // allow returning 0 as null pointer
            if (func_ret && func_ret->kind == TY_PTR && node->lhs->kind == ND_NUM && node->lhs->val == 0) return;
            // allow integer conversion on return
            if (is_integer_type(node->lhs->ty) && func_ret && is_integer_type(func_ret)) return;
            error_at_node(node, "return type mismatch");
        }
        return; }
    case ND_SEQ:
        check(node->lhs, vars, func_ret);
        check(node->rhs, vars, func_ret);
        return;
    case ND_IF:
        check(node->lhs, vars, func_ret);
        if (!node->lhs || !is_integer_type(node->lhs->ty)) error_at_node(node, "if condition must be integer");
        check(node->rhs, vars, func_ret);
        check(node->els, vars, func_ret);
        return;
    case ND_SWITCH:
        check(node->lhs, vars, func_ret);
        if (!node->lhs || !is_integer_type(node->lhs->ty)) error_at_node(node, "switch condition must be integer");
        check(node->rhs, vars, func_ret);
        return;
    case ND_WHILE:
    case ND_DO:
        check(node->lhs, vars, func_ret);
        if (!node->lhs || !is_integer_type(node->lhs->ty)) error_at_node(node, "while condition must be integer");
        check(node->rhs, vars, func_ret);
        return;
    case ND_CASE:
        // case label: ensure constant/integer type expression
        check(node->lhs, vars, func_ret);
        if (!node->lhs || !is_integer_type(node->lhs->ty)) error_at_node(node, "case value must be integer");
        return;
    case ND_DEFAULT:
        // label only
        return;
    case ND_BREAK:
        // semantics enforced during codegen; noop in typechecker
        return;
    case ND_CONTINUE:
        // noop in typechecker; validated at codegen
        return;
    case ND_LABEL:
        // check the labeled statement
        check(node->lhs, vars, func_ret);
        return;
    case ND_GOTO:
        // nothing to typecheck here; label existence validated at function-level
        return;
    case ND_ADD: case ND_SUB: case ND_MUL: case ND_DIV: case ND_MOD:
    case ND_EQ: case ND_NE: case ND_LT: case ND_LE:
    case ND_BITAND: case ND_BITOR: case ND_BITXOR: case ND_SHL: case ND_SHR:
        check_binary(node, vars, func_ret);
        return;
    case ND_LAND: case ND_LOR:
        check(node->lhs, vars, func_ret);
        check(node->rhs, vars, func_ret);
        if (!node->lhs || !is_integer_type(node->lhs->ty)) error_at_node(node, "logical operands must be integers");
        if (!node->rhs || !is_integer_type(node->rhs->ty)) error_at_node(node, "logical operands must be integers");
        node->ty = ty_int; return;
    case ND_NOT:
        check(node->lhs, vars, func_ret);
        if (!is_integer_type(node->lhs->ty)) error_at_node(node, "! operand must be integer");
        node->ty = ty_int; return;
    case ND_BITNOT:
        check(node->lhs, vars, func_ret);
        if (!is_integer_type(node->lhs->ty)) error_at_node(node, "~ operand must be integer");
        node->ty = node->lhs->ty; return;
    case ND_DEREF:
        check(node->lhs, vars, func_ret);
        if (!node->lhs || (node->lhs->ty->kind != TY_PTR && node->lhs->ty->kind != TY_ARRAY)) error_at_node(node, "cannot dereference non-pointer");
        // if array, deref yields element type; if pointer, yields base
        if (node->lhs->ty->kind == TY_PTR) node->ty = node->lhs->ty->base; else node->ty = node->lhs->ty->base;
        return;
    case ND_ADDR:
        // only allow address of variable
        if (node->lhs->kind != ND_VAR) error_at_node(node, "address-of expects variable");
        check(node->lhs, vars, func_ret);
        // cannot take address of 'register' variables
        if (node->lhs->kind == ND_VAR && var_is_register(*vars, node->lhs->name)) error_at_node(node, "cannot take address of register variable");
        node->ty = pointer_to(node->lhs->ty); return;
    case ND_STR:
        node->ty = pointer_to(ty_char);
        return;
    case ND_SIZEOF: {
        // compute at compile time: sizeof operand's type
        if (node->lhs) check(node->lhs, vars, func_ret);
        if (!node->lhs || !node->lhs->ty) error_at_node(node, "sizeof: unknown type");
        int sz = (int)size_of(node->lhs->ty);
        // turn into a literal number node for later stages
        node->kind = ND_NUM;
        node->val = sz;
        node->ty = ty_int;
        return; }
    case ND_CALL: {
        // typecheck args
        int argc = 0;
        for (Node *a = node->lhs; a; a = a->rhs) { check(a, vars, func_ret); argc++; }
        Func *f = find_func(node->name);
        if (!f) {
            // unknown function: assume int return and no checking
            node->ty = ty_int;
            return;
        }
        // check arg count
        int pc = 0;
        for (Node *p = f->params; p; p = p->rhs) pc++;
        if (pc != argc) error_at_node(node, "argument count mismatch in call");
        // check types
        Node *arg = node->lhs;
        for (Node *p = f->params; p; p = p->rhs) {
            if (!arg) break;
            Type *pt = p->ty ? p->ty : ty_int;
            if (!type_equal(pt, arg->ty)) {
                // allow array-to-pointer decay: arg array -> pointer to element
                if (pt->kind == TY_PTR && arg->ty && arg->ty->kind == TY_ARRAY && type_equal(pt, pointer_to(arg->ty->base))) { arg = arg->rhs; continue; }
                // allow integer conversions
                if (is_integer_type(pt) && is_integer_type(arg->ty)) { arg = arg->rhs; continue; }
                if (!(pt->kind == TY_PTR && arg->kind == ND_NUM && arg->val == 0)) error_at_node(node, "argument type mismatch");
            }
            arg = arg->rhs;
        }
        node->ty = f->ret ? f->ret : ty_int;
        return; }
    case ND_FUNC:
        // new var table for function body; add params
        {
            Var *locs = NULL;
            for (Node *p = node->params; p; p = p->rhs) {
                // each param is ND_DECL with name and ty
                Type *pt = p->ty ? p->ty : ty_int;
                if (p->name) locs = var_push(locs, p->name, pt, p->is_register, p->is_restrict);
            }
            // debug: print params and their types when verbose
            extern int opt_verbose;
            if (opt_verbose) {
                for (Node *p = node->params; p; p = p->rhs) {
                    fprintf(stderr, "[typify] param %s ty=%d\n", p->name ? p->name : "(anon)", p->ty ? p->ty->kind : (TypeKind)-1);
                }
            }
            // collect labels/gotos and validate
            NameList *labs = NULL, *gots = NULL;
            collect_labels_and_gotos(node->body, &labs, &gots);
            for (NameList *g = gots; g; g = g->next) {
                int found = 0;
                for (NameList *l = labs; l; l = l->next) { if (strcmp(l->name, g->name) == 0) { found = 1; break; } }
                if (!found) error_at_node(node, "goto to undefined label '%s'", g->name);
            }
            // free simple lists (name strings only)
            for (NameList *it = labs; it; ) { NameList *n = it; it = it->next; free(n->name); free(n); }
            for (NameList *it = gots; it; ) { NameList *n = it; it = it->next; free(n->name); free(n); }

            check(node->body, &locs, node->ty ? node->ty : ty_int);
        }
        return;
    default:
        error_at_node(node, "unhandled node in type checker");
    }
}

// simple table for compile-time constants (enum values)
typedef struct ConstEntry ConstEntry;
struct ConstEntry { char *name; long val; ConstEntry *next; };
static ConstEntry *consts = NULL;
static void consts_add(const char *name, long val) { ConstEntry *c = calloc(1, sizeof(ConstEntry)); c->name = strdup(name); c->val = val; c->next = consts; consts = c; }
static int consts_lookup(const char *name, long *out) { for (ConstEntry *c = consts; c; c = c->next) if (strcmp(c->name, name) == 0) { *out = c->val; return 1; } return 0; }

// helper: evaluate a compile-time integer expression (limited set)
static int eval_const_expr(Node *n, long *out) {
    if (!n) return 0;
    long a, b;
    switch (n->kind) {
    case ND_NUM: *out = n->val; return 1;
    case ND_ADD: if (eval_const_expr(n->lhs, &a) && eval_const_expr(n->rhs, &b)) { *out = a + b; return 1; } return 0;
    case ND_SUB:
        if (n->lhs == NULL) { if (eval_const_expr(n->rhs, &b)) { *out = -b; return 1; } return 0; }
        if (eval_const_expr(n->lhs, &a) && eval_const_expr(n->rhs, &b)) { *out = a - b; return 1; } return 0;
    case ND_MUL: if (eval_const_expr(n->lhs, &a) && eval_const_expr(n->rhs, &b)) { *out = a * b; return 1; } return 0;
    case ND_DIV: if (eval_const_expr(n->lhs, &a) && eval_const_expr(n->rhs, &b) && b != 0) { *out = a / b; return 1; } return 0;
    case ND_BITAND: if (eval_const_expr(n->lhs, &a) && eval_const_expr(n->rhs, &b)) { *out = a & b; return 1; } return 0;
    case ND_BITOR: if (eval_const_expr(n->lhs, &a) && eval_const_expr(n->rhs, &b)) { *out = a | b; return 1; } return 0;
    case ND_BITXOR: if (eval_const_expr(n->lhs, &a) && eval_const_expr(n->rhs, &b)) { *out = a ^ b; return 1; } return 0;
    case ND_SHL: if (eval_const_expr(n->lhs, &a) && eval_const_expr(n->rhs, &b)) { *out = a << b; return 1; } return 0;
    case ND_SHR: if (eval_const_expr(n->lhs, &a) && eval_const_expr(n->rhs, &b)) { *out = a >> b; return 1; } return 0;
    case ND_BITNOT: if (eval_const_expr(n->lhs, &a)) { *out = ~a; return 1; } return 0;
    case ND_VAR: {
        long v; if (consts_lookup(n->name, &v)) { *out = v; return 1; } return 0; }
    default: return 0;
    }
}

void type_check(struct Node *node) {
    // first pass: register all functions and globals
    for (Node *n = node; n; n = n->rhs) {
        if (n->kind == ND_FUNC) add_func_entry(n->name, n->ty ? n->ty : ty_int, n->params, n->body != NULL, n);
        if (n->kind == ND_DECL && n->is_global) {
            // add to globals table (allow forward reference between globals)
            globals = var_push(globals, n->name, n->ty ? n->ty : ty_int, 0, 0);
        }
    }

    // collect compile-time constants from enum-like globals
    for (Node *n = node; n; n = n->rhs) {
        if (n->kind == ND_DECL && n->is_global && n->is_const && n->lhs) {
            long v;
            if (eval_const_expr(n->lhs, &v)) consts_add(n->name, v);
        }
    }

    // second pass: typecheck global initializers
    for (Node *n = node; n; n = n->rhs) {
        if (n->kind == ND_DECL && n->is_global) {
            if (n->lhs) {
                // handle array initializer lists and string-initializers specially
                Type *lt = n->ty ? n->ty : ty_int;
                if (lt && lt->kind == TY_ARRAY) {
                    // string literal initializer for char arrays
                    if (n->lhs->kind == ND_STR && lt->base && lt->base->kind == TY_CHAR) { continue; }
                    // initializer list
                    if (n->lhs->kind == ND_INIT) {
                        int count = 0;
                        for (Node *e = n->lhs->lhs; e; e = e->rhs) {
                            check(e, &globals, NULL);
                            if (!e->ty) error_at_node(n, "unknown type in array initializer");
                            if (!type_equal(e->ty, lt->base) && !(is_integer_type(e->ty) && is_integer_type(lt->base))) error_at_node(n, "array initializer type mismatch");
                            count++;
                        }
                        if (count > (int)lt->len) error_at_node(n, "too many initializers for array");
                        continue;
                    }
                }
                // fallback: check simple scalar initializer
                check(n->lhs, &globals, NULL);
                Type *rt = n->lhs ? n->lhs->ty : NULL;
                if (!rt) error_at_node(n, "unknown type in global initializer");
                if (type_equal(lt, rt)) continue;
                // allow integer conversions for globals
                if (is_integer_type(lt) && is_integer_type(rt)) continue;
                // allow assigning 0 as null pointer
                if (lt->kind == TY_PTR && n->lhs->kind == ND_NUM && n->lhs->val == 0) continue;
                error_at_node(n, "type mismatch in global initializer");
            }
        }
    }

    // third pass: typecheck function definitions
    for (Node *n = node; n; n = n->rhs) {
        if (n->kind == ND_FUNC) check(n, NULL, NULL);
    }
}
