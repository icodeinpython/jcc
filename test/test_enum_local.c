int main() {
    enum { X = 7, Y };
    return Y - 8; // expect 0
}
