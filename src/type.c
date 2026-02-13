#include <stdlib.h>
#include <string.h>
#include "../include/type.h"

Type *ty_int;
Type *ty_char;
Type *ty_short;
Type *ty_long;
Type *ty_longlong;
Type *ty_void;
// unsigned base types
Type *ty_uint;
Type *ty_uchar;
Type *ty_ushort;
Type *ty_ulong;
Type *ty_ulonglong;
Type *ty_bool;

Type *new_type(TypeKind kind) {
    Type *t = calloc(1, sizeof(Type));
    t->kind = kind;
    // LP64 model for x86_64: int=4, short=2, long=8, long long=8, pointer=8, char=1
    if (kind == TY_INT || kind == TY_UINT) t->size = 4;
    else if (kind == TY_SHORT || kind == TY_USHORT) t->size = 2;
    else if (kind == TY_LONG || kind == TY_ULONG) t->size = 8;
    else if (kind == TY_LONGLONG || kind == TY_ULONGLONG) t->size = 8;
    else if (kind == TY_CHAR || kind == TY_UCHAR) t->size = 1;
    else if (kind == TY_PTR) t->size = 8; // LP64 pointers
    else t->size = 1;
    return t;
}

Type *pointer_to(Type *base) {
    Type *t = new_type(TY_PTR);
    t->base = base;
    return t;
}

Type *array_of(Type *base, size_t len) {
    Type *t = new_type(TY_ARRAY);
    t->base = base;
    t->len = len;
    if (base) t->size = size_of(base) * len;
    else t->size = 0;
    return t;
}

// tag table for named structs
typedef struct Tag Tag;
struct Tag { char *name; Type *ty; Tag *next; };
static Tag *tags = NULL;

// find or create a struct type for a tag name (may be incomplete if not yet defined)
Type *struct_named_type(const char *tag) {
    for (Tag *t = tags; t; t = t->next) if (strcmp(t->name, tag) == 0) return t->ty;
    Tag *nt = calloc(1, sizeof(Tag)); nt->name = strdup(tag); nt->ty = new_type(TY_STRUCT); nt->ty->tag = strdup(tag); nt->next = tags; tags = nt; return nt->ty;
}

// define struct members for an existing tag type; computes offsets and size/alignment
void struct_define(Type *st, Member *members) {
    if (!st) return;
    if (st->kind != TY_STRUCT) return;
    // attach members
    st->members = members;
    // compute offsets with natural alignment (member size) and track max alignment
    size_t offset = 0; size_t max_align = 1;
    for (Member *m = members; m; m = m->next) {
        size_t s = size_of(m->ty);
        size_t align = s ? s : 1;
        if (align > max_align) max_align = align;
        // align offset
        size_t rem = offset % align; if (rem) offset += (align - rem);
        m->offset = offset;
        offset += s;
    }
    // round struct size to max_align
    size_t rem = offset % max_align; if (rem) offset += (max_align - rem);
    st->size = offset;
}

size_t size_of(Type *ty) {
    if (!ty) return 0;
    return ty->size;
}

int type_equal(Type *a, Type *b) {
    if (a == b) return 1;
    if (!a || !b) return 0;
    if (a->kind != b->kind) return 0;
    if (a->kind == TY_PTR) return type_equal(a->base, b->base);
    if (a->kind == TY_ARRAY) return a->len == b->len && type_equal(a->base, b->base);
    if (a->kind == TY_STRUCT) {
        // same struct if same tag or same pointer
        if (a->tag && b->tag) return strcmp(a->tag, b->tag) == 0;
        return a == b;
    }
    return 1;
}

__attribute__((constructor)) static void init_types(void) {
    ty_int = new_type(TY_INT);
    ty_char = new_type(TY_CHAR);
    ty_short = new_type(TY_SHORT);
    ty_long = new_type(TY_LONG);
    ty_longlong = new_type(TY_LONGLONG);
    ty_void = new_type(TY_VOID);
    ty_uint = new_type(TY_UINT);
    ty_uchar = new_type(TY_UCHAR);
    ty_ushort = new_type(TY_USHORT);
    ty_ulong = new_type(TY_ULONG);
    ty_ulonglong = new_type(TY_ULONGLONG);
    ty_bool = new_type(TY_BOOL);
}
