int f(register int r) { return r + 1; }

int main() {
    register int x = 2;
    return f(x) - 3; /* expected 0 */
}
