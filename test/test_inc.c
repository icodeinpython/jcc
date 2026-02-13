int puts(char*);

int main() {
    int a = 3;
    int b = ++a; // a=4 b=4
    if (a != 4) return 1;
    if (b != 4) return 2;

    int c = 5;
    int d = c++; // c=6 d=5
    if (c != 6) return 3;
    if (d != 5) return 4;

    int e = 7;
    int f = --e; // e=6 f=6
    if (e != 6) return 5;
    if (f != 6) return 6;

    int g = 9;
    int h = g--; // g=8 h=9
    if (g != 8) return 7;
    if (h != 9) return 8;

    // pointer increment
    char *s = "abc";
    char *p = s;
    char *q = p++;
    if (q != s) return 9;
    if (*p != 'b') return 10;

    return 0;
}
