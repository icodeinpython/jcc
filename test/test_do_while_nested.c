int main() {
    int outer = 0;
    int inner = 0;
    do {
        outer++;
        int j = 0;
        do {
            j++;
            if (j == 2) continue; // skip increment for j==2
            inner++;
            if (j == 3) break; // break inner when j==3
        } while (j < 4);
    } while (outer < 2);
    // For each outer iteration: j runs 1..3, inner increments for j==1 and j==3 -> 2 per outer -> total 4
    return inner - 4;
}
