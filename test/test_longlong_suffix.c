int main() {
    long long a = 123LL;
    long long b = 456ll;
    if (a + b != 579) return 1;
    if (sizeof(long long) != 8) return 2;
    return 0;
}
