// phi interpreter
// Does exactly what the Perl interpreter does, but in native code (and
// therefore much faster).

#include <alloca.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#ifdef DEBUG
#  define GC_malloc malloc
#else
#  include "gc.h"
#endif


size_t minsize(size_t a, size_t b) { return a < b ? a : b; }


// Trivial hash function (for fast symbol compares)
// This is just murmurhash3-64's finalizer over and over again. I don't really
// care about most hashing properties, I just want something that won't collide.
uint64_t hash(size_t const n, char const *const data)
{
  uint64_t h = 0;
  size_t   i = 0;

  for (; i + 7 < n; i += 8)
  {
    h ^= *((uint64_t*)(&data[i]));
    h ^= h >> 33;
    h *= 0xff51afd7ed558ccd;
    h ^= h >> 33;
    h *= 0xc4ceb9fe1a85ec53;
    h ^= h >> 33;
  }

  for (; i < n; ++i)
  {
    h *= 33;
    h += data[i];
  }

  return h;
}


struct phival_t;

struct phinil_t  {};
struct phicons_t { struct phival_t *h, *t; };
struct phiint_t  { int64_t v; };
struct phistr_t  { size_t size; char data[1]; };
struct phisym_t  { uint64_t h; struct phistr_t const *s; };
struct phimut_t  { struct phival_t *v; };

enum phival_type_t
{
  NIL  = 0,
  CONS = 1,
  INT  = 2,
  STR  = 3,
  SYM  = 4,
  MUT  = 5,
};

struct phival_t
{
  enum phival_type_t type;
  union
  {
    struct phinil_t  nil;
    struct phicons_t cons;
    struct phiint_t  integer;
    struct phistr_t  str;
    struct phisym_t  sym;
    struct phimut_t  mut;
  };
};

struct phii_t
{
  struct phival_t *d;
  struct phival_t *c;
  struct phival_t *r;
};

typedef struct phival_t phival;
typedef struct phii_t   phii;


phii the_interpreter;


// Debugging
void print(FILE *f, phival const *const v)
{
  switch (v->type)
  {
    case NIL: fprintf(f, "nil"); break;
    case CONS:
      fprintf(f, "(");
      print(f, v->cons.h);
      fprintf(f, " :: ");
      print(f, v->cons.t);
      fprintf(f, ")");
      break;

    case INT: fprintf(f, "%ld", v->integer.v); break;
    case STR: fprintf(f, "\"%s\"", v->str.data); break;
    case SYM: fprintf(f, "'%s", v->sym.s->data); break;
    case MUT: fprintf(f, "M[...]"); break;

    default:
      fprintf(f, "##INVALID[%d]", v->type);
      break;
  }
}


void die(char const *const error)
{
  fprintf(stderr, "%s\n", error);
  exit(1);
}


void assert(int cond)
{
  if (!cond)
  {
    fprintf(stderr, "\nASSERTION FAILED\n");
    fprintf(stderr, "  d = "); print(stderr, the_interpreter.d); fprintf(stderr, "\n");
    fprintf(stderr, "  c = "); print(stderr, the_interpreter.c); fprintf(stderr, "\n");
    fprintf(stderr, "  r = "); print(stderr, the_interpreter.r); fprintf(stderr, "\n");
    fprintf(stderr, "\n");
    exit(1);
  }
}


phival the_nil = { .type = NIL };

phival *cons(phival *const h, phival *const t)
{
  phival *c = GC_malloc(sizeof(phival));
  c->type = CONS;
  c->cons.h = h;
  c->cons.t = t;
  return c;
}

phival *integer(int64_t const v)
{
  phival *i = GC_malloc(sizeof(phival));
  i->type = INT;
  i->integer.v = v;
  return i;
}

phival *str(size_t const size, char const *const data)
{
  phival *s = GC_malloc(sizeof(phival) + size + 1);
  s->type = STR;
  memcpy(s->str.data, data, size);
  s->str.data[size] = 0;
  s->str.size = size;
  return s;
}

phival *zerostr(size_t const size)
{
  phival *s = GC_malloc(sizeof(phival) + size + 1);
  s->type = STR;
  memset(s->str.data, 0, size);
  s->str.data[size] = 0;
  s->str.size = size;
  return s;
}

phival *sym(struct phistr_t const *const str)
{
  phival *s = GC_malloc(sizeof(phival));
  s->type = SYM;
  s->sym.s = str;
  s->sym.h = hash(str->size, str->data);
  return s;
}

phival *mut(void)
{
  phival *m = GC_malloc(sizeof(phival));
  m->type = MUT;
  return m;
}

phival *deref(phival *v)
{
  while (v->type == MUT) v = v->mut.v;
  return v;
}


phival *cons_tail(phival *v)
{
  v = deref(v);
  assert(v->type == CONS);
  return v->cons.t;
}

phival *cons_head(phival *v)
{
  v = deref(v);
  assert(v->type == CONS);
  return v->cons.h;
}


// Type symbols
phival *type_syms[6];

phival TRUE  = { .type = INT, .integer = { .v = 1 } };
phival FALSE = { .type = INT, .integer = { .v = 0 } };


// Interpreter functions
void dpush(phii *i, phival *v) { i->d = cons(v, i->d); }
phival *dpop(phii *i)
{
  assert(i->d->type == CONS);
  phival *v = i->d->cons.h;
  i->d = deref(i->d->cons.t);
  return v;
}

int64_t dpopint(phii *i)
{
  phival *v = deref(dpop(i));
  assert(v->type == INT);
  return v->integer.v;
}

struct phistr_t *dpopstr(phii *i)
{
  phival *v = deref(dpop(i));
  assert(v->type == STR);
  return &v->str;
}

void cpush(phii *i, phival *v) { i->c = cons(v, i->c); }
phival *cpop(phii *i)
{
  assert(i->c->type == CONS);
  phival *v = i->c->cons.h;
  i->c = deref(i->c->cons.t);
  return v;
}

phival *dpeek(phii *i)
{
  assert(i->d->type == CONS);
  return i->d->cons.h;
}


// Restacking
phival *nthhead(int64_t n, phival *v)
{
  assert(n >= 0);
  while (n--) v = cons_tail(v);
  return cons_head(v);
}

int list_length(phival *v)
{
  int n = 0;
  for (; v->type == CONS; ++n, v = deref(cons_tail(v)));
  return n;
}

void restack(phii *interp)
{
  int64_t n  = dpopint(interp);
  phival *is = dpop(interp);
  assert(n >= 0);

  phival *const head = interp->d;
  phival       *tail = head;
  while (n--) tail = deref(cons_tail(tail));

  int const n_is = list_length(is);
  phival **xs = alloca(sizeof(phival*) * n_is);

  int i = 0;
  for (; is->type == CONS; ++i, is = deref(cons_tail(is)))
  {
    phival *const x = deref(cons_head(is));
    assert(x->type == INT);
    assert(x->integer.v >= 0);
    xs[i] = nthhead(x->integer.v, head);
  }

  while (i--) tail = cons(xs[i], tail);
  interp->d = tail;
}


// Eval delegates
// What happens when each type of value is evaluated?
phival i_eval = { .type = INT, .integer = { .v = 2 } };

void eval(phii *i, phival *v)
{
  phival *a, *b, *c;
  size_t total;
  ssize_t written;

  v = deref(v);
  switch (v->type)
  {
    case NIL:
    case CONS:
    case STR:
      dpush(i, v);
      break;

    case SYM:
      dpush(i, v);
      cpush(i, &i_eval);
      cpush(i, i->r);
      break;

    case INT:
      switch (v->integer.v)
      {
#       define unimplemented(n) \
          case n: fprintf(stderr, "unimplemented instruction %d\n", (n)); \
                  exit(1); \
                  break

#       define binop(n, op) \
          case n: a = dpop(i); \
                  b = dpop(i); \
                  assert(a->type == INT); \
                  assert(b->type == INT); \
                  dpush(i, integer(a->integer.v op b->integer.v)); \
                  break

#       define unop(n, op) \
          case n: dpush(i, integer(op(dpopint(i)))); \
                  break

        case 0x00: dpush(i, cons(i->d, cons(i->c, cons(i->r, &the_nil)))); break;
        case 0x01: i->c = dpop(i); break;
        case 0x02: cpush(i, dpop(i)); break;
        case 0x03: dpush(i, type_syms[dpop(i)->type]); break;
        case 0x04: die("unplemented instruction 4"); break;
        case 0x05: a = dpop(i); b = dpop(i); dpush(i, cons(a, b)); break;
        case 0x06: a = deref(dpop(i)); assert(a->type == CONS);
                   dpush(i, a->cons.t); dpush(i, a->cons.h); break;

        case 0x07: restack(i); break;
        case 0x08: dpush(i, mut()); break;
        case 0x09: a = dpop(i); b = dpeek(i); assert(b->type == MUT);
                   b->mut.v = a; break;

        case 0x0a: i->d = dpop(i); break;
        case 0x0b: i->r = dpop(i); break;

        // Integer ops
        binop(0x10, +);
        unop( 0x11, -);
        binop(0x12, *);
        unimplemented(0x13);
        binop(0x14, <<);
        binop(0x15, >>);
        binop(0x16, &);
        binop(0x17, ^);
        unop( 0x18, ~);
        binop(0x19, <);
        unop( 0x1a, !);

        // String ops
        case 0x20: dpush(i, zerostr((size_t) dpopint(i))); break;
        case 0x21: dpush(i, integer(dpopstr(i)->size));    break;
        case 0x22: a = dpop(i); assert(a->type == STR);
                   b = dpop(i); assert(b->type == INT);
                   assert(b->integer.v >= 0 && b->integer.v < a->str.size);
                   dpush(i, integer(a->str.data[b->integer.v])); break;

        case 0x23: a = dpop(i);  assert(a->type == INT);
                   b = dpop(i);  assert(b->type == INT);
                   c = dpeek(i); assert(c->type == STR);
                   assert(a->integer.v >= 0 && a->integer.v < 256);
                   assert(b->integer.v >= 0 &&
                          b->integer.v < a->str.size);
                   c->str.data[b->integer.v] = (char) a->integer.v;
                   break;

        case 0x24: a = dpop(i); assert(a->type == STR);
                   b = dpop(i); assert(b->type == STR);
                   dpush(i, integer(memcmp(a->str.data, b->str.data,
                                           minsize(a->str.size, b->str.size))));
                   break;

        case 0x25: dpush(i, sym(dpopstr(i))); break;

        case 0x26: a = dpop(i); assert(a->type == SYM);
                   dpush(i, str(a->sym.s->size, a->sym.s->data)); break;

        case 0x27: a = dpop(i); assert(a->type == SYM);
                   b = dpop(i); assert(b->type == SYM);
                   dpush(i, a->sym.h == b->sym.h
                             && a->sym.s->size == b->sym.s->size
                             && !memcmp(a->sym.s->data,
                                        b->sym.s->data,
                                        a->sym.s->size)
                            ? &TRUE
                            : &FALSE); break;

        case 0x28: a = dpop(i); assert(a->type == STR);
                   b = dpop(i); assert(b->type == STR);
                   c = zerostr(a->str.size + b->str.size);
                   memcpy(c->str.data, a->str.data, a->str.size);
                   memcpy(c->str.data + a->str.size, b->str.data, b->str.size);
                   dpush(i, c); break;

        case 0x40: dpush(i, integer(0)); break;
        case 0x41: die("crashed!"); break;

        // Custom extensions
        case 0x100:                     // print string to stdout
          a = dpop(i); assert(a->type == STR);
          total = 0;
          while ((written = write(1, a->str.data + total,
                                     a->str.size - total)) > 0)
            total += written;

          if (total < a->str.size)
            die("print instruction failed to write full string");

          break;

        case 0x101:                     // print thing to stdout
          print(stdout, dpop(i));
          fflush(stdout);
          break;

        default:
          fprintf(stderr, "unknown instruction %ld\n", v->integer.v);
          exit(1);
          break;
      }
      break;

    default:
      fprintf(stderr, "unknown value type %d\n", v->type);
      exit(1);
      break;
  }
}


// Interpreter stepping
void cpack(phii *i)
{
  while (i->c->type == CONS && deref(cons_head(i->c))->type == NIL)
    i->c = deref(cons_tail(i->c));
}

phival *next_insn(phii *i)
{
  cpack(i);
  phival *h = deref(cons_head(i->c));
  phival *t = deref(cons_tail(i->c));
  if (h->type == CONS)
  {
    phival *hh = deref(cons_head(h));
    phival *ht = deref(cons_tail(h));
    t = cons(ht, t);
    h = hh;
  }
  i->c = t;
  cpack(i);
  return h;
}

int has_next_insn(phii *i) { cpack(i); return i->c->type == CONS; }
void step(phii *i) { eval(i, next_insn(i)); }
void run(phii *i)  { while (has_next_insn(i)) step(i); }


// Value loader
// Load values from packed-binary format. This is pretty straightforward. Here's
// what this looks like, with <ref> and <size> being native-endian uint32s and
// <n> being a native-endian int64:
//
//   00 = nil
//   01 = cons(<ref>, <ref>)
//   02 = int(<n>)
//   03 = str(<size>, <bytes...>)
//   04 = sym(<str-ref>)
//   05 = mut(<ref>)
//
// Each value in the image gets assigned an index, and that's what <ref> refers
// to. For example, if we wanted to cons the list [1 2 3] on a little-endian
// system:
//
//   00                     # index = 0: nil
//   02 0300000000000000    # index = 1: 3
//   01 00000000 01000000   # index = 2: cons(3, nil)
//   02 0200000000000000    # index = 3: 2
//   01 03000000 02000000   # index = 4: cons(2, cons(3, nil))
//   02 0100000000000000    # index = 5: 1
//   01 05000000 04000000   # index = 6: cons(1, cons(2, cons(3, nil)))
//
// NB: muts can contain forward references! These are resolved after the fact.
// muts are the only type of value that can refer forwards.

phival *load_binary(size_t const n, char const *const d)
{
  // Allocate space for the table of muts. These are 1:1 for possible values
  // right now, which is fine because the allocation isn't persistent.
  uint32_t *mut_table = alloca(n / 5 * sizeof(uint32_t));
  uint32_t *mut_vals  = alloca(n / 5 * sizeof(uint32_t));
  int       n_muts    = 0;

  // What's the largest number of values we can have? If we say that every value
  // must be referred to by someone, then I think we get something like n / 5:
  // that would be a string of circular muts.
  phival **vs    = alloca(n / 5 * sizeof(phival*));
  int      n_vs  = 0;

  uint32_t x;
  for (size_t o = 0; o + 1 < n;)
    switch (d[o++])
    {
      case 0: vs[n_vs++] = &the_nil; break;
      case 1: vs[n_vs++] = cons(vs[*(uint32_t*)(&d[o])],
                                vs[*(uint32_t*)(&d[o + 4])]);
              o += 8;
              break;

      case 2: vs[n_vs++] = integer(*(int64_t*)(&d[o])); o += 8; break;
      case 3:
        vs[n_vs++] = str(x = *(uint32_t*)(&d[o]), &d[o + 4]);
        o += 4 + x;
        break;

      case 4:
        x = *(uint32_t*)(&d[o]);
        assert(vs[x]->type == STR);
        vs[n_vs++] = sym(&vs[x]->str);
        o += 4;
        break;

      case 5:
        mut_table[n_muts]  = n_vs;
        mut_vals[n_muts++] = *(uint32_t*)(&d[o]);
        vs[n_vs++] = mut();
        o += 4;
        break;

      default:
        fprintf(stderr, "invalid image byte: %d\n", d[o - 1]);
        exit(1);
        break;
    }

  // Fix up muts
  for (int i = 0; i < n_muts; ++i)
  {
    assert(mut_vals[i] < n_vs);
    vs[mut_table[i]]->mut.v = vs[mut_vals[i]];
  }

  // Return is always the largest value
  return vs[n_vs - 1];
}


int main(int argc, char **argv)
{
  // Set up type syms
  phival *nil_type_str  = str(3, "nil");
  phival *cons_type_str = str(4, "cons");
  phival *int_type_str  = str(3, "int");
  phival *str_type_str  = str(3, "str");
  phival *sym_type_str  = str(3, "sym");
  phival *mut_type_str  = str(3, "mut");

  phival *nil_type  = sym(&nil_type_str->str);
  phival *cons_type = sym(&cons_type_str->str);
  phival *int_type  = sym(&int_type_str->str);
  phival *str_type  = sym(&str_type_str->str);
  phival *sym_type  = sym(&sym_type_str->str);
  phival *mut_type  = sym(&mut_type_str->str);

  type_syms[NIL]  = nil_type;
  type_syms[CONS] = cons_type;
  type_syms[INT]  = int_type;
  type_syms[STR]  = str_type;
  type_syms[SYM]  = sym_type;
  type_syms[MUT]  = mut_type;

  // Read the image from stdin
  uint32_t image_size = 0;
  if (read(0, &image_size, 4) <= 0) die("failed to read image size");

  assert(image_size < 1048576);
  char *image = malloc(image_size);
  uint32_t offset = 0;
  ssize_t n_read = 0;
  while ((n_read = read(0, image + offset, image_size - offset)) > 0)
    offset += n_read;

  if (offset < image_size) die("failed to read entire image");

  // Decode the image into a value
  phival *v = load_binary(image_size, image);
  free(image);

  the_interpreter.d = &the_nil;
  the_interpreter.c = &the_nil;
  the_interpreter.r = &the_nil;

  dpush(&the_interpreter, v);
  cpush(&the_interpreter, &i_eval);
  run(&the_interpreter);

  return 0;
}
