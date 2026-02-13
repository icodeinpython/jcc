struct S { int a; int b; };
int main() { struct S x; return sizeof(x) - 8; }
