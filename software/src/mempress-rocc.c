//see LICENSE for license
// The following is a RISC-V program to test the functionality of the
// sha3 RoCC accelerator.
// Compile with riscv-gcc sha3-rocc.c
// Run with spike --extension=sha3 pk a.out

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <assert.h>
#include <malloc.h>
#include "rocc.h"
#include "compiler.h"
#include "encoding.h"

#ifdef __linux
#include <sys/mman.h>
#endif
 
#define MAX_STREAMS 16
#define ALIGN_BYTES 1
#define CL_BYTES 64

enum STREAM_TYPE {
    STRIDE_RD = 0,
    STRIDE_WR = 1,
    BURST_RD = 2,
    BURST_WR = 3,
    RAND_RD = 4,
    RAND_WR = 5
};

// can't we do argv argc
int main() {
  unsigned long cycle_cnt, req_sent;
  int ii, jj, kk;

  int stride_bytes[MAX_STREAMS];
  int stride_idx[MAX_STREAMS];
  for (ii = 0; ii < MAX_STREAMS; ii++) {
      stride_bytes[ii] = (ii + 1) * CL_BYTES;
      stride_idx[ii] = stride_bytes[ii] / (int)sizeof(char);
  }

  int kB = 1024;
  int l2_kB = 2048;
  int max_reqs = l2_kB * kB / (CL_BYTES * 1); // stride 0 accesses 2MB range
/* int max_reqs = l2_kB / stride_bytes[0]; // stream 0 accesses 2MB range */

  int stream_cnt = 1;

  enum STREAM_TYPE stream_type[MAX_STREAMS];
  for (ii = 0; ii < MAX_STREAMS; ii++) {
      int r = ii % 2;
      if (r == 0) stream_type[ii] = STRIDE_RD;
      else stream_type[ii] = STRIDE_WR;
  }

  int mem_size = 0;
  int addr_offset[MAX_STREAMS];
  int idx_offset[MAX_STREAMS];
  for (ii = 0; ii < stream_cnt; ii++) {
      addr_offset[ii] = mem_size;
      idx_offset[ii] = addr_offset[ii] / (int)sizeof(char);
      mem_size += (max_reqs * stride_bytes[ii]);
  }
  int max_idx = mem_size / (int)sizeof(char);

  assert(stream_cnt <= MAX_STREAMS);
  char* input = (char*)memalign(sizeof(char), (size_t)mem_size);

  // initialize the input
  // this touches all the data so that mempress will not incur page faults
  for (ii = 0; ii < max_idx; ii++) {
      input[ii] = '3'; // 0011_0011
  }

#ifdef __linux
  // Ensure all pages are resident to avoid accelerator page faults
  if (mlockall(MCL_CURRENT | MCL_FUTURE)) {
    perror("mlockall");
    return 1;
  }
#endif

  do {
    printf("Start basic test\n");

    // insert fence instruction
    asm volatile ("fence");

    // number of streams && rd or wr
    ROCC_INSTRUCTION_SS(2, stream_cnt, max_reqs, 1);
    for (ii = 0; ii < stream_cnt; ii++) {
        uint64_t stride_n_type = (stride_bytes[ii] << 3) | ((int)stream_type[ii]);
        ROCC_INSTRUCTION_SS(2, stride_n_type, &input[idx_offset[ii]], 2);
    }

    // get hw counter values
    ROCC_INSTRUCTION_D(2, cycle_cnt, 3);
    ROCC_INSTRUCTION_D(2, req_sent, 4);

    // remove fence instruction
    asm volatile ("fence" ::: "memory");

    assert(cycle_cnt != 0);

    int bytes_sent = req_sent * 16;
    int nano_sec = cycle_cnt / 2; // assuming 2.0 GHz .... float support missing?
    int bw_MBps = (bytes_sent * 1000) / nano_sec;

    printf("cycle_cnt value: %lu\n", cycle_cnt);
    printf("req_sent value: %lu\n", req_sent);
    printf("Achieved BW of the system: %d MB/s\n", bw_MBps);

/* for (ii = 0; ii < stream_cnt; ii++) { */
/* for (jj = 0; jj < max_reqs; jj++) { */
/* for (kk = 0; kk < 16; kk++) { */
/* printf("input[%d][%d][%d]: %d ", */
/* ii, jj, kk, */
/* input[idx_offset[ii] + jj * stride_idx[ii] + kk]); */
/* } */
/* printf("\n"); */
/* } */
/* } */
  } while(0);

  printf("Execution Finished!\n");
  return 0;
}
