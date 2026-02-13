int puts(char*);

char c = 5;
short s = -1;
long l = 1000;

int main() {
    if (c != 5) return 1;
    if (s != -1) return 2;
    if (l != 1000) return 3;
    return 0;
}
