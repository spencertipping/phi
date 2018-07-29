// Does this compile with "gcc -c"? If so, C unifies types for ?: alternatives.
// (Yep, it works)

struct foo
{
  int bif;
};

struct foo *f1(void);
struct foo *f2(void);

int condition;

int bar(void)
{
  return (condition ? f1() : f2())->bif;
}
