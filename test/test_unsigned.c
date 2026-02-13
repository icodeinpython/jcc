int main() {
    // test basic unsigned arithmetic and comparisons
    unsigned a = 1;
    unsigned b = 2;
    if (a + b != 3) return 1;

    // unsigned underflow wraps around -> should be > 0
    unsigned c = a - 2;
    if (!(c > 0)) return 2;

    // literal suffix parsed as unsigned
    unsigned d = 0u;
    if (d != 0) return 3;

    // signed to unsigned conversion: -1 becomes large positive unsigned
    int s = -1;
    unsigned su = s;
    if (!(su > 0)) return 4;

    // unsigned division
    unsigned e = 5;
    if (e / 2 != 2) return 5;

    return 0;
}
