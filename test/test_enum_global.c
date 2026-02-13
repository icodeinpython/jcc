enum { A, B = 5, C };
int main() {
    return C - 6; // expect 0
}
