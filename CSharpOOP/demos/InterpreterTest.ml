(** Copyright 2021-2022, Pavel Alimov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Csharpoop_lib.KeyMap
open Csharpoop_lib.Parser
open Csharpoop_lib.ClassLoader.ClassLoader (Csharpoop_lib.ResultMonad.Result)
open Csharpoop_lib.Interpreter.Interpretation (Csharpoop_lib.ResultMonad.Result)

let interpret_test program class_table =
  match load program class_table with
  | Error message -> print_endline message
  | Ok loaded -> (
      match execute loaded with
      | Error message -> print_endline message
      | Ok result -> print_endline (show_context result ^ "\n"))

let () =
  print_string
    "-_-_-_-_-_-_-_-_-_-_- Simple interpretation testing -_-_-_-_-_-_-_-_-_-_-\n\n"

let program =
  Option.get
    (apply parser
       {|
public class Program
{
  public static void Main() 
  {
    int a = 1;
    int b = 2;
    int c = 3;
  }  
}
|})

let () = interpret_test program KeyMap.empty

let () =
  print_string
    "-_-_-_-_-_-_-_-_-_-_- Arithmetic testing -_-_-_-_-_-_-_-_-_-_-\n\n"

let program =
  Option.get
    (apply parser
       {|
public class Program
{
  public static void Main()
  {
    int a = 1;
    int b = 2;
    int c = 3;
    int val1 = 1 + 2 + 3 + 4 + 5;
    int val2 = a + b;
    int val3 = a + 100;
    int val4 = 10 / 2;
    int val5 = 10 % 2;
    int val6 = (a + b) * 100; 
    a = a + 1;
    int val7 = (val1 * val2 + 4) / 2 + 100; 
    string s1 = "a";
    string s2 = "b";
    string s3 = s1 + s2;
    string s4 = s1 + a;
    string s5 = a + s2;
  }  
}
|})

let () = interpret_test program KeyMap.empty

let () =
  print_string
    "-_-_-_-_-_-_-_-_-_-_- Bool expressions testing -_-_-_-_-_-_-_-_-_-_-\n\n"

let program =
  Option.get
    (apply parser
       {|
public class Program
{
  public static void Main()
  {    
    int a = 10, b = 50, c = 100;
    if (b > a)
    {
      Console.WriteLine("b greater than a");
    }
    if (a < c)
    {
      Console.WriteLine("a less than c");
    } 
    if (b <= c)
    {
      Console.WriteLine("b less or equal than c");
    }
    if (c >= b)
    {
      Console.WriteLine("c greater or equal than b");
    }
    int d = 10;
    if (d == a)
    {
      Console.WriteLine("d equal to a");
    }
    if (a != b)
    {
      Console.WriteLine("b not equal to a");
    }
    if (a < b && b < c)
    {
      Console.WriteLine("a less than b and b less than c");
    }
    if (a < b || a != 10)
    {
      Console.WriteLine("a less than b or a not equal to 10");
    }
    if (!(a >= c))
    {
      Console.WriteLine("not (a greater or equal to c)");
    }
    string s1 = "a", s2 = "b";
    if (s1 == "a")
    {
      Console.WriteLine("s1 equal to 'a'");
    }
    if (s2 != "a")
    {
      Console.WriteLine("s2 not equal to 'a'");
    }
    Person p1 = new Person(20, "Bob"), p2 = new Person(30, "Alice"), p3 = p1;
    if (p1 != p2)
    {
      Console.WriteLine("p1 not equal to p2");
    }
    if (p1 == p3)
    {
      Console.WriteLine("p1 equal to p3");
    }
  }  
}

public class Person
{
  int age;
  string name;

  public Person(int age, string name)
  {
    this.age = age;
    this.name = name;
  }
}
|})

let () = interpret_test program KeyMap.empty

let () =
  print_string
    "-_-_-_-_-_-_-_-_-_-_- Method call testing -_-_-_-_-_-_-_-_-_-_-\n\n"

let program =
  Option.get
    (apply parser
       {|
public class Program
{
  public static void Main()
  {
    Person person = new Person(25, "Bob");
    int res = person.Sum(25, 100);
    int a1 = person.GetAge();
    person.SetAge(30);
    int a2 = person.GetAge(); 
    Console.WriteLine("Old age = " + a1);
    Console.WriteLine("New age = " + a2);
  }
}

public class Person
{
  int age;
  string name;

  public Person()
  {
    
  }

  public Person(int age, string name)
  {
    this.age = age;
    this.name = name;
  }

  public int Sum(int a, int b)
  {
    return a + b;
  }

  public int GetAge()
  {
    return this.age;
  }

  public void SetAge(int age)
  {
    this.age = age;
  }
}
|})

let () = interpret_test program KeyMap.empty

let () =
  print_string
    "-_-_-_-_-_-_-_-_-_-_- Update object state testing -_-_-_-_-_-_-_-_-_-_-\n\n"

let program =
  Option.get
    (apply parser
       {|
public class Program
{
  public static void Main()
  {
    Person person = new Person(25, "Bob");
    Person p1, p2, p3;
    p1 = person;
    p2 = p1;
    p3 = p2;
    person.SetAge(55);
    int res = p2.GetAge(); 
    Console.WriteLine(res);
  }
}

public class Person
{
  int age;
  string name;

  public Person()
  {

  }

  public Person(int age, string name)
  {
    this.age = age;
    this.name = name;
  }

  public int GetAge()
  {
    return this.age;
  }

  public void SetAge(int age)
  {
    this.age = age;
  }
}
|})

let () = interpret_test program KeyMap.empty

let () =
  print_string
    "-_-_-_-_-_-_-_-_-_-_- Inheritance testing -_-_-_-_-_-_-_-_-_-_-\n\n"

let program =
  Option.get
    (apply parser
       {|
public class Program
{
  public static void Main()
  {
    Person person = new Person(25, "Bob");
    Person childFirst = new Child(3, "Alice");
    Child childSecond = new Child(person);
    Console.WriteLine("Child second default age = " + childSecond.GetAge());
    childSecond.SetAge(20);
    Console.WriteLine("Child second new age = " + childSecond.GetAge());
    childFirst.SetAge(4);
    person.SetAge(27);
    Console.WriteLine("Parent new age = " + person.GetAge());
    Console.WriteLine("Child first new age = " + childFirst.GetAge());
    Console.WriteLine("Child second new parent age  = " + childSecond.GetAge());
  }
}

public class Person
{
  protected int age;
  protected string name;

  public Person()
  {

  }

  public Person(int age, string name)
  {
    this.age = age;
    this.name = name;
  }

  public int GetAge()
  {
    return this.age;
  }

  public void SetAge(int age)
  {
    this.age = age;
  }
}

public class Child : Person
{
  public Person parent;

  public Child(int age, string name) : base(age, name)
  {
    parent = new Person(40, "Spike");
  }

  public Child(Person parent)
  {
    this.parent = parent;
  }

  public Child()
  {

  }

  public string GetName()
  {
    return name;
  }

  public Person GetParent()
  {
    return parent;
  }
}
|})

let () = interpret_test program KeyMap.empty

let () =
  print_string "-_-_-_-_-_-_-_-_-_-_- Scope testing -_-_-_-_-_-_-_-_-_-_-\n\n"

let program =
  Option.get
    (apply parser
       {|
public class Program
{
  public static void Main()
  {
    int a = 2;
    int b = 3;
    int c = 4;
    if (c == 4) 
    {
      int d = 7;
      a = a + 1;
      int e = 10;
      int g = 42;
    }
    b = 5;
    c = 6;
    a = 15;
    int i = 0;
    while (i < 3)
    {
      int m = 2;
      int n = 3;
      int z = 4;
      int f = 5;
      i = i + 1;
    }
    a = 100;
    b = 200;
    c = 300;
    for (int k = 0, p = 0; k < 6; k++)
    {
      int m = 2;
      int n = 3;
      int z = 4;
      int f = 5;
      p = p + 1;
    }
    a = 1000;
    b = 2000;
    c = 3000;
  }
}   
|})

let () = interpret_test program KeyMap.empty

let () =
  print_string
    "-_-_-_-_-_-_-_-_-_-_- Many loops testing -_-_-_-_-_-_-_-_-_-_-\n\n"

let program =
  Option.get
    (apply parser
       {|
public class Program
{
  public static void Main()
  {
    int n = 4;
    for (int i = 0; i < n - 1; i++)
    {
      for (int j = 0; j < n - i - 1; j++)
      {
        if (j > i)
        {
          int temp = i + j;
          Console.WriteLine("temp = " + temp);
        }
      }
    }
  }
}
|})

let () = interpret_test program KeyMap.empty

let () =
  print_string
    "-_-_-_-_-_-_-_-_-_-_- Break and continue testing -_-_-_-_-_-_-_-_-_-_-\n\n"

let program =
  Option.get
    (apply parser
       {|
public class Program
{
  public static void Main()
  {
    for (int i = 0; i < 10; ++i)
    {
      if (i % 2 == 0)
      {
        continue;
      }
      Console.WriteLine("i = " + i);
    }
    Console.WriteLine("#####");
    for (int i = 0; i < 10; ++i)
    {
      Console.WriteLine("i = " + i);
      if (i == 5)
      {
        break;
      }
    } 
  }
}
|})

let () = interpret_test program KeyMap.empty

let () =
  print_string
    "-_-_-_-_-_-_-_-_-_-_- Object state changing in another context testing \
     -_-_-_-_-_-_-_-_-_-_-\n\n"

let program =
  Option.get
    (apply parser
       {|
public class Program
{
  public static void Main()
  {
    Person person = new Person(25, "Bob");
    Child child = new Child(person);
    child.SetParentAge(30);
    Console.WriteLine(person.GetAge());
  }
}

public class Person
{
    public int age;
    public string name;

    public Person()
    {

    }

    public Person(int age, string name)
    {
      this.age = age;
      this.name = name;
    }

    public int GetAge()
    {
      return age;
    }
}

public class Child : Person
{
  public Person parent;

  public Child(int age, string name) : base(age, name)
  {
    parent = new Person(40, "Spike");
  }

  public Child(Person parent)
  {
    this.parent = parent;
  }

  public void SetParentAge(int age)
  {
    Person p1 = parent; 
    p1.age = age;
  }
}
|})

let () = interpret_test program KeyMap.empty

let () =
  print_string
    "-_-_-_-_-_-_-_-_-_-_- Factorial recursion testing -_-_-_-_-_-_-_-_-_-_-\n\n"

let program =
  Option.get
    (apply parser
       {|
public class Program
{
  public static void Main()
  {
    Factorial factorial = new Factorial();
    int f = factorial.GetFact(5);
    Console.WriteLine(factorial.GetFact(5));
    Console.WriteLine(f);
  }
}

public class Factorial
{
  public int GetFact (int n)
  {
    if (n <= 1) return 1;
    else return n * GetFact(n - 1);
  }
}
|})

let () = interpret_test program KeyMap.empty

let () =
  print_string
    "-_-_-_-_-_-_-_-_-_-_- Constructor chaining testing -_-_-_-_-_-_-_-_-_-_-\n\n"

let program =
  Option.get
    (apply parser
       {|
public class Program
{
  public static void Main()
  {
    Cat cat = new Cat(2, "Mars", 30);
  }
}

public class Pet
{
  public int age;
  public string name;

  public Pet(int age, string name)
  {
    this.age = age;
    this.name = name;
  }
}

public class Cat : Pet
{
  public int hairLevel;

  public Cat(int age, string name) : base(age, name)
  {

  }

  public Cat(int age, string name, int hairLevel) : this(age, name)
  {
    this.hairLevel = hairLevel;
  }
}
|})

let () = interpret_test program KeyMap.empty

let () =
  print_string
    "-_-_-_-_-_-_-_-_-_-_- Constructor chaining recursion testing \
     -_-_-_-_-_-_-_-_-_-_-\n\n"

let program =
  Option.get
    (apply parser
       {|
public class Program
{
  public static void Main()
  {
    Cat cat = new Cat(2, "Mars", 30);
  }
}

public class Pet
{
  public int age;
  public string name;

  public Pet(int age, string name)
  {
    this.age = age;
    this.name = name;
  }
}

public class Cat : Pet
{
    public int hairLevel;

    public Cat(int age, string name) : base(age, name)
    {

    }

    public Cat(int age, string name, int hairLevel) : this(age, name, hairLevel)
    {
      
    }
}
|})

let () = interpret_test program KeyMap.empty

let () =
  print_string
    "-_-_-_-_-_-_-_-_-_-_- Const fields testing -_-_-_-_-_-_-_-_-_-_-\n\n"

let program =
  Option.get
    (apply parser
       {|
public class Program
{
  public static void Main()
  {
    Math m = new Math();
    m.PI = 4;
  }
}

public class Math
{
  public const int PI = 3;
}
|})

let () = interpret_test program KeyMap.empty

let () =
  print_string
    "-_-_-_-_-_-_-_-_-_-_- Const variables testing -_-_-_-_-_-_-_-_-_-_-\n\n"

let program =
  Option.get
    (apply parser
       {|
public class Program 
{
  public static void Main()
  {
    const int a = 25;
    a++;
  }
}
|})

let () = interpret_test program KeyMap.empty

let () =
  print_string
    "-_-_-_-_-_-_-_-_-_-_- Ad-hoc polymorphism, specifically methods \
     overloading, testing -_-_-_-_-_-_-_-_-_-_-\n\n"

let program =
  Option.get
    (apply parser
       {|
public class Program
{
  public static void Main()
  {
    Summator summator = new Summator();
    int a = summator.Sum(5, 3);
    string s = summator.Sum("F", "P"); 
    Console.WriteLine("5 + 3 = " + a);
    Console.WriteLine("F + P = " + s);
  }
}

public class Summator
{
  public int Sum(int a, int b)
  {
    return a + b;
  }

  public string Sum(string a, string b)
  {
    return a + b;
  }
}
|})

let () = interpret_test program KeyMap.empty

let () =
  print_string "-_-_-_-_-_-_-_-_-_-_- Cast test -_-_-_-_-_-_-_-_-_-_-\n\n"

let program =
  Option.get
    (apply parser
       {|
       public class Program
       {
         public static void Main() 
         {
           SomeClass b = new SomeClass();
           int a = (int)b.F();
           Console.WriteLine(a);
         }
       }

       public class SomeClass
       {
         public int F()
         {
           return 123;
         }
       }
|})

let () = interpret_test program KeyMap.empty

let () =
  print_string "-_-_-_-_-_-_-_-_-_-_- Cast value test -_-_-_-_-_-_-_-_-_-_-\n\n"

let program =
  Option.get
    (apply parser
       {|
       public class Program
       {
         public static void Main() 
         {
           int a = (int)1;
           Console.WriteLine(a);
           bool b = (bool)1;
           Console.WriteLine(b);
         }
       }
|})

let () = interpret_test program KeyMap.empty

let () =
  print_string
    "-_-_-_-_-_-_-_-_-_-_- Cast variable test -_-_-_-_-_-_-_-_-_-_-\n\n"

let program =
  Option.get
    (apply parser
       {|
       public class Program
       {
         public static void Main() 
         {
           int a = (int)1;
           bool b = (bool)false;
           Console.WriteLine(a);
           Console.WriteLine(b);
           int c = (int)a;
           Console.WriteLine(c);
         }
       }
|})

let () = interpret_test program KeyMap.empty

let () =
  print_string "\n-_-_-_-_-_-_-_-_-_-_- Cast to class -_-_-_-_-_-_-_-_-_-_-\n\n"

let program =
  Option.get
    (apply parser
       {|
       public class Program
       {
         public static void Main() 
         {
          SomeClass a = new SomeClass();
          Console.WriteLine(a.F());
          a = (SomeClass)a;
          Console.WriteLine(a.F());
          SomeClass b = (SomeClass)a;
          Console.WriteLine(b.F());
          int c = (int)b.F();
          Console.WriteLine(c);
         }
       }

       public class SomeClass
       {
         public int F()
         {
           return 123;
         }
       }
|})

let () = interpret_test program KeyMap.empty

let () =
  print_string
    "\n\
     -_-_-_-_-_-_-_-_-_-_- Check access to non-visible members \
     -_-_-_-_-_-_-_-_-_-_-\n\n"

let program =
  Option.get
    (apply parser
       {|
       public class Program
       {
         public static void Main() 
         {
          A a = new A();
          B b = new B();
          Console.WriteLine("a.f() = " + a.F());
          Console.WriteLine("b.f() = " + b.F());
          Console.WriteLine("b.g() = " + b.G());
          A c = new B();
          Console.WriteLine("c.f() = " + c.F());
          Console.WriteLine("c.g() = " + c.G());
         }
       }

       public class A
       {
         public int F()
         {
           return 123;
         }
       }

       public class B : A
       {
         public int G()
         {
           return 321;
         }
       }
|})

let () = interpret_test program KeyMap.empty

let () =
  print_string
    "\n-_-_-_-_-_-_-_-_-_-_- Check access modifiers -_-_-_-_-_-_-_-_-_-_-\n\n"

let program =
  Option.get
    (apply parser
       {|
       public class Program
       {
         public static void Main() 
         {
          A a = new A();
          Console.WriteLine("a.f() = " + a.F());
          Console.WriteLine("a.public = " + a.Pub);
          B b = new B();
          Console.WriteLine("b.f() = " + b.F());
          Console.WriteLine("b.public = " + b.Pub);
          Console.WriteLine("b.GetProt() = " + b.GetProt());
          Console.WriteLine("b.private = " + b.Priv);
         }
       }

       public class A
       {
          public int Pub = 10; 
          protected int Prot = 11; 
          private int Priv = 12; 

          private int F()
          {
            return 123;
          }
       }

       public class B : A
       {
         public int GetProt()
         {
           return this.Prot;
         }
       }
|})

let () = interpret_test program KeyMap.empty
