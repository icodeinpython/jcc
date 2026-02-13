int puts(char*);

long long foo() { long long x = 123456789; return x + 1; }

int main() {
    long long x = 123456789012345;
    long long y = -1;
    if (sizeof(long long) != 8) return 1;
    if (foo() == 0) return 2;
    if (y != -1) return 3;
    return 0;
}
