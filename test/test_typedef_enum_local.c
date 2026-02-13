int main() {
    typedef enum { B = 3 } F;
    if (B != 3) return 1;
    F y = B;
    if (y != 3) return 2;
    return 0;
}
