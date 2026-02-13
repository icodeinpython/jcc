int main() {
    // basic bit ops
    if ((6 & 3) != 2) return 1;
    if ((6 | 3) != 7) return 2;
    if ((6 ^ 3) != 5) return 3;
    if ((~0) != -1) return 4; // ~0 == -1 (all bits set)
    if ((1 << 3) != 8) return 5;
    if ((16 >> 2) != 4) return 6;
    unsigned int x = 1u << 31;
    if ((x >> 31) != 1u) return 7; // logical shift for unsigned
    // compound assignments
    int a = 5; a &= 3; if (a != (5 & 3)) return 8;
    int b = 5; b |= 2; if (b != (5 | 2)) return 9;
    int c = 5; c ^= 1; if (c != (5 ^ 1)) return 10;
    int d = 1; d <<= 4; if (d != 16) return 11;
    int e = 16; e >>= 2; if (e != 4) return 12;
    return 0;
}