#include <stdio.h>

int f(int x)
{
  return x + 1;
}

int main()
{
  int i;
  int x;

  printf("starting at label, here's a listing\n");
  for (i = 0; i < 20; ++i)
    printf("  l[%d] = %x\n", i, ((unsigned char*)&&label)[i]);

label:
  x = f(5);
}
