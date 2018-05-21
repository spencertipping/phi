package foo
{
  our $AUTOLOAD;
  sub AUTOLOAD
  {
    print "$AUTOLOAD called\n";
  }
}

(bless {}, 'foo')->4;
