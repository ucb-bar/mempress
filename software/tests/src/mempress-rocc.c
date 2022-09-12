//see LICENSE for license
// The following is a RISC-V program to test the functionality of the
// sha3 RoCC accelerator.
// Compile with riscv-gcc sha3-rocc.c
// Run with spike --extension=sha3 pk a.out

#include <stdio.h>
#include <stdint.h>
#include <assert.h>
#include "rocc.h"
#include "compiler.h"
#include "encoding.h"

#ifdef __linux
#include <sys/mman.h>
#endif
 
#define MAX_ELEMS 4096
#define MAX_STREAMS 4
#define ALIGN_BYTES 1
#define CL_BYTES 64


int main() {
  unsigned long start, end;
  int ii, jj;

  int stream_cnt = 4;
  int stream_type = 1; // 0 is read, otherwise write
  int max_reqs = 13;
  int stride_bytes;   // access stride in bytes
  int stride_idx;

  assert(stream_cnt <= MAX_STREAMS);

#ifdef __linux
  // Ensure all pages are resident to avoid accelerator page faults
  if (mlockall(MCL_CURRENT | MCL_FUTURE)) {
    perror("mlockall");
    return 1;
  }
#endif

  do {
    printf("Start basic test 1.\n");

    // Setup some test data
    static unsigned char input[MAX_STREAMS * MAX_ELEMS] __aligned(ALIGN_BYTES * 8) = {'\0'}; // __aligned(x) -> aligns x bits...?

    start = rdcycle();

    asm volatile ("fence");

    ROCC_INSTRUCTION_SS(2, stream_cnt, stream_type, 0); // number of streams && rd or wr
    ROCC_INSTRUCTION_S(2, max_reqs, 1);                 // max req per stream
    for (ii = 0; ii < stream_cnt; ii++) {
        stride_bytes = (ii + 1) * CL_BYTES;
        stride_idx = stride_bytes / ALIGN_BYTES;

        ROCC_INSTRUCTION_SS(2, stride_bytes, &input[ii * MAX_ELEMS], 2); // stride in bytes, start address
    }
    assert(stream_cnt * CL_BYTES / ALIGN_BYTES * max_reqs <= MAX_ELEMS);
    asm volatile ("fence" ::: "memory");

    end = rdcycle();

    for (ii = 0; ii < stream_cnt; ii++) {
        stride_bytes = (ii + 1) * CL_BYTES;
        stride_idx = stride_bytes / ALIGN_BYTES;

        for (jj = 0; jj < max_reqs * stride_idx; jj += stride_idx) {
            printf("input[%d][%d]: %d\n", ii, jj,
                    input[ii * MAX_ELEMS + jj]);
        }
    }
  } while(0);

  printf("Execution Finished!\n");
  printf("MemPress execution took %lu cycles\n", end - start);

  return 0;
}
