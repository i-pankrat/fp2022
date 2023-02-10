Simple variable assign
  $ ./demoInterpret.exe <<-"EOF"
  > x=1 + 3 * 2
  > x
  > EOF
  (LiteralValue (IntLit 7))

Interesting variable assign (equal to a=10, 1+28 * a)
  $ ./demoInterpret.exe <<-"EOF"
  > 1+28*a=10
  > [a]
  > EOF
  (ArrayValue [(LiteralValue (IntLit 10))])

Ruby values
  $ ./demoInterpret.exe <<-"EOF"
  > [1, true, 'st r?!', nil, [1,2]]
  > EOF
  (ArrayValue
     [(LiteralValue (IntLit 1)); (LiteralValue (BoolLit true));
       (LiteralValue (StringLit "st r?!")); (LiteralValue NilLit);
       (ArrayValue [(LiteralValue (IntLit 1)); (LiteralValue (IntLit 2))])])

Ruby if-else-elsif
  $ ./demoInterpret.exe <<-"EOF"
  > a = if true then 2 end
  > b = if false then 2 end
  > x=0
  > c=if (while x < 5 do x=x+1 end) == nil then 5 end
  > [a,b,c,x]
  > EOF
  (ArrayValue
     [(LiteralValue (IntLit 2)); (LiteralValue NilLit);
       (LiteralValue (IntLit 5)); (LiteralValue (IntLit 0))])

Builtin operations (indexing for int)
  $ ./demoInterpret.exe <<-"EOF"
  > a=3
  > [a[0], a[1], a[2], a[3]]
  > EOF
  (ArrayValue
     [(LiteralValue (IntLit 1)); (LiteralValue (IntLit 1));
       (LiteralValue (IntLit 0)); (LiteralValue (IntLit 0))])

Builtin operations (indexing for arrays)
  $ ./demoInterpret.exe <<-"EOF"
  > a=[1,2,3]
  > [a[0], a[1], a[2]]
  > EOF
  (ArrayValue
     [(LiteralValue (IntLit 1)); (LiteralValue (IntLit 2));
       (LiteralValue (IntLit 3))])

Builtin operations (indexing for strings)
  $ ./demoInterpret.exe <<-"EOF"
  > a='abcdef'
  > [a[0], a[0,2], a[3,3]]
  > EOF
  (ArrayValue
     [(LiteralValue (StringLit "a")); (LiteralValue (StringLit "ab"));
       (LiteralValue (StringLit "def"))])

Builtin operations (length)
  $ ./demoInterpret.exe <<-"EOF"
  > a='abcdef'
  > b=[1,2,3]
  > [a.length(), b.length()]
  > EOF
  (ArrayValue [(LiteralValue (IntLit 6)); (LiteralValue (IntLit 3))])

Functions (simple method)
  $ ./demoInterpret.exe <<-"EOF"
  > def methodname()
  >   a=10
  > end
  > methodname()
  > EOF
  (LiteralValue (IntLit 10))

Functions (lambda, that can use 'outer' scope)
  $ ./demoInterpret.exe <<-"EOF"
  > b=2
  > y=lambda {|x|1+b}
  > y(10)
  > EOF
  (LiteralValue (IntLit 3))

Factorial (with loop)
  $ ./demoInterpret.exe <<-"EOF"
  > def fact(n)
  >   acc = 1
  >   while n > 0 do
  >     acc = acc * n
  >     n = n - 1
  >   end
  >   acc
  > end
  > fact(5)
  > EOF
  (LiteralValue (IntLit 120))

Factorial (recursive)
  $ ./demoInterpret.exe <<-"EOF"
  > def fact(n) if n <= 1 then n else n * fact(n - 1) end end
  > fact(5)
  > EOF
  (LiteralValue (IntLit 120))

Factorial (using Y-combinator)
  $ ./demoInterpret.exe <<-"EOF"
  > def Y(g)
  >  g(lambda { Y(g) })
  > end
  > fact = Y(lambda {|g| lambda {|n| if n == 0 then 1 else n * (g())(n - 1) end}})
  > fact(5)
  (LiteralValue (IntLit 120))

Classes (class declaration returns value!)
  $ ./demoInterpret.exe <<-"EOF"
  > class Pair
  >   def initialize(a,b)
  >       @b=a
  >       @a=b
  >   end
  >   def get_a()
  >       @b
  >   end
  >   def get_b()
  >       @a
  >   end
  > end
  > EOF
  (MethodValue ("get_b", ([], (Var "@a"))))

Classes (creating instance and method invoking)
  $ ./demoInterpret.exe <<-"EOF"
  > class Pair
  >   def initialize(a,b)
  >       @b=a
  >       @a=b
  >   end
  >   def get_a()
  >       @b
  >   end
  >   def get_b()
  >       @a
  >   end
  > end
  > p=Pair.new(2,3)
  > [p.get_a(), p.get_b()]
  > EOF
  (ArrayValue [(LiteralValue (IntLit 2)); (LiteralValue (IntLit 3))])
