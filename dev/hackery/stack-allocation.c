int f(int x)
{
  if (x > 0)
  {
    long xs[1024];
    return xs[1023];
  }
  else
  {
    char xs[1024];
    return xs[1023];
  }
}

int main(int argc, char **argv)
{
  return f(argc);
}
