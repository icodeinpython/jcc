typedef enum { A = 5 } E;

int main() {
    if (A != 5) return 1;
    E x = A;
    if (x != 5) return 2;
    return 0;
}
