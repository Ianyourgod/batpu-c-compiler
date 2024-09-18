struct Hi {
    int a;
    struct Hi* b;
};

static void itakeastruct(struct Hi* hi) {
    hi->a = 5;
}

static int main() {
    struct Hi hi = {0, 0};
    itakeastruct(&hi);
    return hi.a;
}