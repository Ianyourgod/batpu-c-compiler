int mult(int a, int b) {
    int res = 0;
    while (a > 0) {
        res += b;
        a--;
    }
    return res;
}