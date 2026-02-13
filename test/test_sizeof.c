int puts(char*);

int main() {
    int a = sizeof(int);
    int b = sizeof(char);
    int c = sizeof(long);
    int d = sizeof(char*);
    if (a != 4) return 1;
    if (b != 1) return 2;
    if (c != 8) return 3;
    if (d != 8) return 4;
    return 0;
}
