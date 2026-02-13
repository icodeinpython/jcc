int puts(char*);

int main() {
    int a = 1;
    a += 2; if (a != 3) return 1;
    a -= 1; if (a != 2) return 2;
    a *= 3; if (a != 6) return 3;
    a /= 2; if (a != 3) return 4;
    a %= 2; if (a != 1) return 5;

    unsigned int u = 5u; u %= 2u; if (u != 1u) return 6;
    long long b = 10ll; b /= 4ll; if (b != 2ll) return 7;
    int c = -5; c %= 2; if (c != -1) return 8;

    char *s = "abcd";
    char *p = s;
    p += 2; if (*p != 'c') return 9;

    return 0;
}