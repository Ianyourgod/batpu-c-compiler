struct A {
    int a;
    struct A *b;
};

static int main() {
    struct A b = { 3 };
    struct A a = { 5, &b };
    return a.b->a;
}

