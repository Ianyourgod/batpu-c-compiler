/* Make sure stack arguments are deallocated correctly after returning from a function call; also test passing variables as stack arguments */

#ifdef SUPPRESS_WARNINGS
#pragma GCC diagnostic ignored "-Wunused-parameter"
#endif

int five() {
    return 5;
}

void unneeded() {}

int lots_of_args(int a, int b, int c, int d, int e, int f, int g, int h, int i, int j, int k, int l, int m, int n, int o) {
    int temp = five();
    int temp2 = five();
    int wowza = five();
    unneeded();
    return wowza + o;
}

int main(void) {
    return lots_of_args(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15);
}