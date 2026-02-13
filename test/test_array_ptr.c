int add(int *p) { return p[1]; }
int main() {
    int a[2];
    a[1] = 42;
    return add(a) - 42;
}
