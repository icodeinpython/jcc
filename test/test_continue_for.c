int main() {
    int sum = 0;
    for (int i = 0; i < 5; i++) {
        if (i % 2 == 0) continue;
        sum += i;
    }
    if (sum != (1 + 3)) return 1;
    return 0;
}
