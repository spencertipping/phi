// Do processors with a 48-bit address space ignore the other bits for TLB
// lookups?
//
// Nope. This program segfaults.

#include <stdio.h>

int main()
{
  int  x  = 10;
  int *xp = &x;

  printf("memory[%llx] = %d\n", (long long) xp, *xp);
  xp = (int*) ((long long) xp | 1ll << 63);
  printf("memory[%llx] = %d\n", (long long) xp, *xp);
  return 0;
}
