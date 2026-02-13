int main() {
    // continue in for-loop: skip even numbers
    int sum = 0;
    for (int i = 0; i < 5; i++) {
        if (i % 2 == 0) continue;
        sum += i;
    }
    if (sum != (1 + 3)) return 1;

    // continue in while-loop: skip inc when condition holds
    int x = 0;
    int y = 0;
    while (x < 5) {
        x++;
        if (x % 2 == 0) continue;
        y += x;
    }
    // y should be 1+3+5 = 9
    if (y != 9) return 2;

    return 0;
}
