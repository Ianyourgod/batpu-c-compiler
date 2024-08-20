int mult(int a, int b) {
    int result = 0;
    for (int i = 0; i < b; i++) {
        result = result + a;
    }
    return result;
}

int other_func() {
    int skibidi_toilet_money_mouth = 2;
    return mult(skibidi_toilet_money_mouth + 3, 4);
}

int main() {
    return other_func();
}