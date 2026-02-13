_Bool f(_Bool x) { return x; }

int main() {
    _Bool b = 2; // should become 1
    if (b != 1) return 1;
    b = 0;
    if (b != 0) return 2;
    b = 3 + 0; // 3 -> 1
    if (b != 1) return 3;
    _Bool c = b && 0; // 1 && 0 -> 0
    if (c != 0) return 4;
    if (f(2) != 1) return 5;
    return 0;
}