int main() {
    int x = 2;
    int y = 0;
    switch (x) {
        case 1: y = 10; break;
        case 2: y = 20; break;
        default: y = 30;
    }
    if (y != 20) return 1;

    // fall-through
    x = 1; y = 0;
    switch (x) {
        case 1: y = 1;
        case 2: y = 2; break;
    }
    if (y != 2) return 2;

    // break inside while should exit loop
    x = 0; y = 0;
    while (1) {
        x++;
        if (x == 3) break;
    }
    if (x != 3) return 3;

    return 0;
}
