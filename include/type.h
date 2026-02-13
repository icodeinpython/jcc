#ifndef TYPE_H
#define TYPE_H

#include <stddef.h>

typedef enum {
    TY_INT,
    TY_CHAR,
    TY_SHORT,
    TY_LONG,
    TY_PTR,
    TY_ARRAY,
    TY_STRUCT,
    TY_VOID,
    TY_LONGLONG,
    // unsigned counterparts
    TY_UINT,
    TY_UCHAR,
    TY_USHORT,
    TY_ULONG,
    TY_ULONGLONG,
    // boolean
    TY_BOOL,
} TypeKind;

typedef struct Type Type;

struct Node;

// member definition for struct types
typedef struct Member Member;
struct Member { char *name; Type *ty; size_t offset; Member *next; };

struct Type {
    TypeKind kind;
    Type *base; // for pointers and arrays (element type)
    size_t size;
    size_t len; // for arrays: number of elements
    // for structs
    Member *members;
    char *tag; // optional tag name for tagged structs
};

extern Type *ty_int;
extern Type *ty_char;
extern Type *ty_short;
extern Type *ty_long;
extern Type *ty_longlong;
extern Type *ty_void;
// unsigned base types
extern Type *ty_uint;
extern Type *ty_uchar;
extern Type *ty_ushort;
extern Type *ty_ulong;
extern Type *ty_ulonglong;
extern Type *ty_bool;

Type *pointer_to(Type *base);
Type *array_of(Type *base, size_t len);
size_t size_of(Type *ty);
int type_equal(Type *a, Type *b);
void type_check(struct Node *node);

// tagged struct helpers
Type *struct_named_type(const char *tag);
void struct_define(Type *st, Member *members);

#endif // TYPE_H
