int puts(char*);

int main() {
    if (5 % 2 != 1) return 1;
    if (5 % -2 != 1) return 2;
    if (-5 % 2 != -1) return 3;
    if (-5 % -2 != -1) return 4;
    unsigned int a = 5u % 2u;
    if (a != 1) return 5;
    long long b = 7ll % 4ll;
    if (b != 3) return 6;
    if (7 % 4 != 3) return 7;
    return 0;
}