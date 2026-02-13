enum { ONE = 1, FOUR = 1 << 2, MASK = ONE | FOUR };
int main() { return MASK - 5; }
