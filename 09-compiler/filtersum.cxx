#include <cstdint>
#include <cstdio>
#include <cstring>

int main() {
    uint64_t sum = 0;
    char f1[16];
    uint64_t f2;
    while (scanf("%s%llu", f1, &f2) == 2) {
        if (strcmp(f1, "42") == 0)
            sum += f2;
    }
    printf("%llu\n", sum);
    return 0;
}
