int main() {
    int i = 0;
    int sum = 0;
    do {
        i++;
        if (i % 2 == 0) continue;
        sum += i;
    } while (i < 5);
    return sum - 9; // 1+3+5 = 9
}
