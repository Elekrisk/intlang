#include <stdio.h>

struct Value {
    unsigned char type;
    unsigned long long data[4];
};

struct BackingStore {
    unsigned long long count;
    struct Value value;
};

struct VarStorage {
    unsigned long long count;
    struct BackingStore* backing_store_list;
};


extern struct VarStorage var_storage;

void initialize_variable_storage(void);
void expand_variable_storage(void);
struct BackingStore* allocate_backing_store(void);

void assert(int cmp) {
    if (!cmp) {
        printf("Assertion failed\n");
    }
}

int main() {
    initialize_variable_storage();
    struct BackingStore* store = allocate_backing_store();
    printf("%p\n", store);
    assert(store == allocate_backing_store());
    for (int i = 0; i < 20; ++i) {
        store = allocate_backing_store();
        printf("%p\n", store);
        store->count++;
    }
}


