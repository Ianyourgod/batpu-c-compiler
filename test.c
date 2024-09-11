static char func(char a) {
    return a;
}

static int main() {
    int s[5] = { 2, 4, 6, 8, 10 };

    return func(s[1]);
}