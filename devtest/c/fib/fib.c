#include <stdio.h>

int fib(int n) {
    if (n < 3) {
        return 1;
    }

    return fib(n-1) + fib(n-2);
}

int main() {
    int x = 30;
    printf("%d\n", fib(x));

}