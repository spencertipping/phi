#include <stdio.h>

typedef void *v;
typedef void(*f)(void);

v d[1024]; int di = 0;
f c[1024]; int ci = 0;

#define dpush(x) (d[di++] = (x))
#define dpop(x)  (d[--di])
#define cpush(x) (c[ci++] = (x))
#define cpop(x)  (c[--ci])

#define i(x) ((void*) ((unsigned long) (x) << 1 | 1))

v const typeof_op = i(1);
v const int_t     = i(2);
v const parser_t  = i(3);

void run(void) { while (1) cpop()(); }

void vtable_run(void)
{
  v val = dpop();
  dpush(val);
  cpush(**(f**)val);
}

// Q: where do arguments go? probably pushed onto the stack ... but then how do
// we keep track of the parse-controlling object?
void ll_vtable_eval(void)
{
  v val     = dpop();
  v message = dpop();

  // The low-level vtable format is a series of int matchers, terminated with a
  // null entry.
  for (v *vtable = (void**) *(void**)val + 1; vtable[1]; vtable += 2)
    if (*vtable == message)
    {
      cpush(((f*) vtable)[1]);
      return;
    }
}

int main()
{
  printf("ok\n");
  return 0;
}
