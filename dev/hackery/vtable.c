#include <stdio.h>

typedef void *v;

v d[1024]; int di = 0;
v c[1024]; int ci = 0;

#define dpush(x) (d[di++] = x)
#define dpop(x)  (d[--di])
#define cpush(x) (c[ci++] = x)
#define cpop(x)  (c[--ci])

#define i(x) ((x) << 1 | 1)

v const typeof_op = i(1);
v const int_t     = i(2);
v const parser_t  = i(3);

// TODO: nope nope nope
#define next_ goto *(void**)cpop()

void eval(v obj)
{
  dpush(obj);
  goto **(void***)obj;
}

void ll_vtable_eval()
{
  v val     = dpop();
  v message = dpop();

  // The low-level vtable format is a series of int matchers, terminated with a
  // null entry.
  for (v *vtable = (void**) *(void**)val + 1;
       vtable[1];
       vtable += 2)
    if (*vtable == message)
      goto *(void**)(vtable + 1);

  next_;
}

int main()
{
  printf("ok\n");
  return 0;
}
