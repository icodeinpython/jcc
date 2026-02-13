int main() {
    int i = 0;
    do {
        i++;
        if (i == 2) break;
    } while (i < 10);
    return i - 2; // expect 0
}
