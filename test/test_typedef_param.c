typedef int I;

int f(I x) { return x + 1; }

int main() {
    if (f(3) != 4) return 1;
    return 0;
}
