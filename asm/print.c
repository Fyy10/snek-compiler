#include <stdio.h>

void print(long int val) {
    printf("raw: %#lx val: %ld\n", val, val >> 1);
}
