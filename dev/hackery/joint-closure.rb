# Let's close over a stack-allocated variable from multiple lambdas. Do they
# share modifications after the initial scope is gone?

# (Yep, ruby does indeed share modifications to jointly-captured variables)

def test_the_thing
  x = 10                        # our captured quantity
  [ Proc.new { x += 1 },
    Proc.new { x -= 1 } ]
end

inc, dec = test_the_thing

puts inc.call
puts inc.call
puts dec.call
puts dec.call
