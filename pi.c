void mem_write(int i, int j);

void print_num(int i, int idx) {
    mem_write(250, i);
    mem_write(241, idx-2);
    for (int j=i;j!=0;j--) {
        mem_write(240, j-1);
        mem_write(242, 0);
        mem_write(245, 0);
    }
}

int main() {
    int N = 5;
    int len = (10 * N / 3) + 1;
    int A[20];

    for (int i = 0; i < len; i++) {
        A[i] = 2;
    }

    int nines = 0;
    int predigit = 0;

    int j = 1;
    for (;j <= N; j++) {
        int q = 0;

        for (int i = len; i > 0; i--) {
            int tmp = A[i - 1] << 3;
            int tmp2 = A[i - 1] << 1;
            int x = tmp + tmp2 + (q * i);
            
            A[i - 1] = x % (2 * i - 1);
            q = x / (2 * i - 1);
        }

        A[0] = q % 10;
        q = q / 10;

        if (q == 9) {
            nines++;
        } else if (q == 10) {
            print_num(predigit + 1, j);
            for (int k = 0; k < nines; k++) {
                print_num(0, j);
            }
            predigit = 0;
            nines = 0;
        } else {
            print_num(predigit, j);
            predigit = q;

            if (nines > 0) {
                for (; nines > 0; nines--) {
                    print_num(9, j);
                }
            }
        }
    }

    //print_num(predigit, j); // this one overflows, so we're not gonna display it for now

    return predigit; // we will, however, return it, for the people who wanna see it.
}