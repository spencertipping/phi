class foo:
  def f(self, x):
    return x + 1

  g = lambda self, y: y + 2

print foo().f(5)
print foo().g(5)
