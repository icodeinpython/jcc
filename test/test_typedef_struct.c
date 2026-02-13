typedef struct S { int a; } S;

int main() {
    S s;
    if (sizeof(s) != sizeof(int)) return 1;
    return 0;
}
