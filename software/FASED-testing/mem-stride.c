// Simple memory stride in order to observe FASED behavior in waveforms

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include <malloc.h>

int main() {
  // array size is 4 x L2 size for HyperscaleMegaBoomFASEDTestConfig
  size_t l2_kB = 64;
  size_t kB = 1024;
  size_t mem_size = l2_kB * kB * 4;
  size_t stride = 64;

  printf("Start allocating memory with memalign\n");
  char *arr = (char *) memalign(sizeof(char), mem_size);

  printf("Prevents page faults by touching them\n");
  size_t page_stride = 4 * kB / sizeof(char);
  for (size_t i = 0; i < mem_size; i += page_stride) {
    arr[i] = '3'; // 0011_0011
  }

  do {
    printf("Start basic test\n");

    // insert fence instruction
    asm volatile ("fence");

    for (size_t i = 0; i < mem_size; i += 64) {
      // two reads and a write
      arr[i] = arr[i-1] + arr[i+1];
    }

    // remove fence instruction
    asm volatile ("fence" ::: "memory");
  } while(0);

  printf("Execution Finished!\n");
  return 0;
}
