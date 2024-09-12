static int add2ToNumber(int *num){
    *num = *num + 2;
}

static int main(){
    int a = 12;
    add2ToNumber(&a);
}