int main() {
    int x = 0;
    goto set;
    x = 1;
set:
    x = 2;
    return x - 2;
}
