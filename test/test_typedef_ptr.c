typedef int *PI;

int main() {
    int v = 5;
    PI p = &v;
    if (*p != 5) return 1;
    return 0;
}
