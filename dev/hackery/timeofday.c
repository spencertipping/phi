// Interesting! No system call for gettimeofday().
// (Update: https://0xax.gitbooks.io/linux-insides/content/SysCall/linux-syscall-3.html)

#include <stdio.h>
#include <sys/time.h>

int main()
{
  struct timeval tv;
  gettimeofday(&tv, NULL);

  printf("timeval map:\n");
  printf("  overall size: %ld\n", sizeof(tv));
  printf("  size of tv.sec:  %ld\n", sizeof(tv.tv_sec));
  printf("  size of tv.usec: %ld\n", sizeof(tv.tv_usec));
  printf("%ld\n", tv.tv_sec);
  return 0;
}
