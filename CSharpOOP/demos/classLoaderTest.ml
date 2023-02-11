(** Copyright 2021-2022, Pavel Alimov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Csharpoop_lib.Ast
open Csharpoop_lib.KeyMap
open Csharpoop_lib.Parser
open Csharpoop_lib.ClassLoader.ClassLoader (Csharpoop_lib.ResultMonad.Result)

let show_hashtbl ht pp_element = KeyMap.pp pp_element Format.std_formatter ht
let show_class_table ht = show_hashtbl ht pp_table_class

let load_test program class_table =
  match load program class_table with
  | Error message -> print_endline message
  | Ok result -> show_class_table result

let () =
  print_string
    "-_-_-_-_-_-_-_-_-_-_- Wrong modifier testing -_-_-_-_-_-_-_-_-_-_-\n\n"

let program =
  Option.get
    (apply parser
       {|
public class Program
{
  public static void Main() 
  {
	}
}

public class A
{
  public private int a;
}
|})

let () = load_test program KeyMap.empty

let () =
  print_string
    "\n-_-_-_-_-_-_-_-_-_-_- Simple object testing -_-_-_-_-_-_-_-_-_-_-\n\n"

let program =
  Option.get
    (apply parser
       {|
public class Program
{
  public static void Main() 
  {
	}
}

public class A
{
  public int a;
  private int Method(int b) {}
}

class B
{
  private int a;
}

public class C
{
  protected int a;
}

public interface D
{
  int a();
}
|})

let () = load_test program KeyMap.empty

let () =
  print_string
    "\n\
     -_-_-_-_-_-_-_-_-_-_- Interface inheritance error testing \
     -_-_-_-_-_-_-_-_-_-_-\n\n"

let program =
  Option.get
    (apply parser
       {|
public class Program
{
  public static void Main() 
  {
	}
}

public interface I
{
  int F();
}

public class A : I
{

}

public class B : I
{
  public int F() {}
}
|})

let () = load_test program KeyMap.empty

let () =
  print_string
    "\n-_-_-_-_-_-_-_-_-_-_- Hide method testing -_-_-_-_-_-_-_-_-_-_-\n\n"

let program =
  Option.get
    (apply parser
       {|
public class Program
{
  public static void Main() 
  {
	}
}


public class A
{
  public int F() 
  {
    return 1;
  }
}

public class B : A
{
  public int F() 
  {
    return 2;
  }
}
|})

let () = load_test program KeyMap.empty

let () =
  print_string
    "\n\
     -_-_-_-_-_-_-_-_-_-_- This constructor key testing -_-_-_-_-_-_-_-_-_-_-\n\n"

let program =
  Option.get
    (apply parser
       {|
public class Program
{
  public static void Main() 
  {
	}
}

public class A
{
  public int a;
  private int Method(int b) {}

  public A()
  {

  }

  public A(int k) : this()
  {
    
  }
}

public class B : A
{
}
|})

let () = load_test program KeyMap.empty

let () =
  print_string
    "\n-_-_-_-_-_-_-_-_-_-_- Inheritance testing -_-_-_-_-_-_-_-_-_-_-\n\n"

let program =
  Option.get
    (apply parser
       {|
public class Program
{
  public static void Main() 
  {
    Person person = new Person(100, 50);
	}
}

public class Person
{
  public int weight;
  public int age;
  
  public Person(int weight, int age) 
  {
    this.weight = weight;
    this.age = age;
  }
  
  public int GetWeight()
  {
    return weight;
  }

  public void SetWeight(int weight)
  {
    this.weight = weight;
  }
  
  public virtual int GetAge()
  {
    return age;
  }

  public void SetAge(int age)
  {
    this.age = age;
  }
}

public class Child : Person
{
  public int cash;
  
  public Child(int weight, int age) : base(weight, age)
  {
    cash = 0;
  }

  public Child(int weight, int age, int cash) : base(weight, age)
  {
    this.cash = cash;
  }

  public override int GetAge()
  {
    return age + 1;
  } 
  
  public int GetCash()
  {
    return cash;
  }
  
  public void SetCash(int cash)
  {
    this.cash = cash;
  }
  
  public void TellEvenNumbers(int count) 
  {
    for (int i = 0; i < count; i++)
    {
      if (i % 2 == 0 && !(i % 2 == 1))
      {
        Console.WriteLine(i);
      }
      else
      {
        continue;
      }
    }
  }
}
|})

let () = load_test program KeyMap.empty

let () =
  print_string
    "\n-_-_-_-_-_-_-_-_-_-_- Wrong modifiers testing -_-_-_-_-_-_-_-_-_-_-\n\n"

let program =
  Option.get
    (apply parser
       {|
public class Program
{
  public static void Main() 
  {
    Person person = new Person(100, 50);
	}
}

public class Person
{
  public int weight;
  public int age;
  
  public Person(int weight, int age) 
  {
    this.weight = weight;
    this.age = age;
  }
  
  public int GetWeight()
  {
    return weight;
  }

  public const void SetWeight(int weight)
  {
    this.weight = weight;
  }
  
  public int GetAge()
  {
    return age;
  }

  public void SetAge(int age)
  {
    this.age = age;
  }
}

public class Child : Person
{
  public int cash;
  
  public Child(int weight, int age) : base(weight, age)
  {
    cash = 0;
  }

  public Child(int weight, int age, int cash) : base(weight, age)
  {
    this.cash = cash;
  }
  
  public int GetCash()
  {
    return cash;
  }
  
  public void SetCash(int cash)
  {
    this.cash = cash;
  }
  
  public void TellEvenNumbers(int count) 
  {
    for (int i = 0; i < count; i++)
    {
      if (i % 2 == 0 && !(i % 2 == 1))
      {
        Console.WriteLine(i);
      }
      else
      {
        continue;
      }
    }
  }
}
|})

let () = load_test program KeyMap.empty

let program =
  Option.get
    (apply parser
       {|
public class Program
{
  public static void Main() 
  {
    Person person = new Person(100, 50);
	}
}

public class Person
{
  public int weight;
  public int age;
  
  public Person(int weight, int age) 
  {
    this.weight = weight;
    this.age = age;
  }
  
  public int GetWeight()
  {
    return weight;
  }

  public void SetWeight(int weight)
  {
    this.weight = weight;
  }
  
  public int GetAge()
  {
    return age;
  }

  public void SetAge(int age)
  {
    this.age = age;
  }
}

static class Child : Person
{
  public int cash;
  
  public Child(int weight, int age) : base(weight, age)
  {
    cash = 0;
  }

  public Child(int weight, int age, int cash) : base(weight, age)
  {
    this.cash = cash;
  }
  
  public int GetCash()
  {
    return cash;
  }
  
  public void SetCash(int cash)
  {
    this.cash = cash;
  }
  
  public void TellEvenNumbers(int count) 
  {
    for (int i = 0; i < count; i++)
    {
      if (i % 2 == 0 && !(i % 2 == 1))
      {
        Console.WriteLine(i);
      }
      else
      {
        continue;
      }
    }
  }
}
|})

let () = load_test program KeyMap.empty

let program =
  Option.get
    (apply parser
       {|
public class Program
{
  public static void Main() 
  {
    Person person = new Person(100, 50);
	}
}

public class Person
{
  public int weight;
  public int age;
  
  public Person(int weight, int age) 
  {
    this.weight = weight;
    this.age = age;
  }
  
  public int GetWeight()
  {
    return weight;
  }

  public void SetWeight(int weight)
  {
    this.weight = weight;
  }
  
  public int GetAge()
  {
    return age;
  }

  public void SetAge(int age)
  {
    this.age = age;
  }
}

class Child : Person
{
  public int cash;
  
  public abstract Child(int weight, int age) : base(weight, age)
  {
    cash = 0;
  }

  public Child(int weight, int age, int cash) : base(weight, age)
  {
    this.cash = cash;
  }
  
  public int GetCash()
  {
    return cash;
  }
  
  public void SetCash(int cash)
  {
    this.cash = cash;
  }
  
  public void TellEvenNumbers(int count) 
  {
    for (int i = 0; i < count; i++)
    {
      if (i % 2 == 0 && !(i % 2 == 1))
      {
        Console.WriteLine(i);
      }
      else
      {
        continue;
      }
    }
  }
}
|})

let () = load_test program KeyMap.empty

let () =
  print_string
    "\n\
     -_-_-_-_-_-_-_-_-_-_- Similar fields error testing -_-_-_-_-_-_-_-_-_-_-\n\n"

let program =
  Option.get
    (apply parser
       {|
public class Program
{
  public static void Main() 
  {
    Person person = new Person(100, 50);
	}
}

public class Person
{
  public int age;
  public int age;
  
  public Person(int weight, int age) 
  {
    this.weight = weight;
    this.age = age;
  }
  
  public int GetWeight()
  {
    return weight;
  }

  public void SetWeight(int weight)
  {
    this.weight = weight;
  }
  
  public int GetAge()
  {
    return age;
  }

  public void SetAge(int age)
  {
    this.age = age;
  }
}

public class Child : Person
{
  public int cash;
  
  public Child(int weight, int age) : base(weight, age)
  {
    cash = 0;
  }

  public Child(int weight, int age, int cash) : base(weight, age)
  {
    this.cash = cash;
  }
  
  public int GetCash()
  {
    return cash;
  }
  
  public void SetCash(int cash)
  {
    this.cash = cash;
  }
  
  public void TellEvenNumbers(int count) 
  {
    for (int i = 0; i < count; i++)
    {
      if (i % 2 == 0 && !(i % 2 == 1))
      {
        Console.WriteLine(i);
      }
      else
      {
        continue;
      }
    }
  }
}
|})

let () = load_test program KeyMap.empty

let () =
  print_string
    "\n\
     -_-_-_-_-_-_-_-_-_-_- Similar methods error testing -_-_-_-_-_-_-_-_-_-_-\n\n"

let program =
  Option.get
    (apply parser
       {|
public class Program
{
  public static void Main() 
  {
    Person person = new Person(100, 50);
	}
}

public class Person
{
  public int weight;
  public int age;
  
  public Person(int weight, int age) 
  {
    this.weight = weight;
    this.age = age;
  }
  
  public int GetWeight()
  {
    return weight;
  }

  public void SetWeight(int weight)
  {
    this.weight = weight;
  }
  
  public int GetAge()
  {
    return age;
  }

  public void SetWeight(int age)
  {
    this.age = age;
  }
}

public class Child : Person
{
  public int cash;
  
  public Child(int weight, int age) : base(weight, age)
  {
    cash = 0;
  }

  public Child(int weight, int age, int cash) : base(weight, age)
  {
    this.cash = cash;
  }
  
  public int GetCash()
  {
    return cash;
  }
  
  public void SetCash(int cash)
  {
    this.cash = cash;
  }
  
  public void TellEvenNumbers(int count) 
  {
    for (int i = 0; i < count; i++)
    {
      if (i % 2 == 0 && !(i % 2 == 1))
      {
        Console.WriteLine(i);
      }
      else
      {
        continue;
      }
    }
  }
}
|})

let () = load_test program KeyMap.empty

let () =
  print_string
    "\n\
     -_-_-_-_-_-_-_-_-_-_- Similar constructors error testing \
     -_-_-_-_-_-_-_-_-_-_-\n\n"

let program =
  Option.get
    (apply parser
       {|
public class Program
{
  public static void Main() 
  {
    Person person = new Person(100, 50);
	}
}

public class Person
{
  public int weight;
  public int height;
  public int age;
  public int income;
  
  public Person(int weight, int age) 
  {
    this.weight = weight;
    this.age = age;
  }

  public Person(int height, int income)
  {
    this.height = height;
    this.income = income;
  }
  
  public int GetWeight()
  {
    return weight;
  }

  public void SetWeight(int weight)
  {
    this.weight = weight;
  }
  
  public int GetAge()
  {
    return age;
  }

  public void SetWeight(int age)
  {
    this.age = age;
  }
}

public class Child : Person
{
  public int cash;
  
  public Child(int weight, int age) : base(weight, age)
  {
    cash = 0;
  }

  public Child(int weight, int age, int cash) : base(weight, age)
  {
    this.cash = cash;
  }
  
  public int GetCash()
  {
    return cash;
  }
  
  public void SetCash(int cash)
  {
    this.cash = cash;
  }
  
  public void TellEvenNumbers(int count) 
  {
    for (int i = 0; i < count; i++)
    {
      if (i % 2 == 0 && !(i % 2 == 1))
      {
        Console.WriteLine(i);
      }
      else
      {
        continue;
      }
    }
  }
}
|})

let () = load_test program KeyMap.empty

let () =
  print_string
    "\n-_-_-_-_-_-_-_-_-_-_- Abstract errors testing -_-_-_-_-_-_-_-_-_-_-\n\n"

let program =
  Option.get
    (apply parser
       {|
public class Program
{
  public static void Main()
  {
    Builder builder = new Builder();
    Circle circle = new Circle();

    circle.build(builder);
	}
}

public class Figure 
{
  public abstract void build(Builder builder);
}

public class Circle : Figure
{
  public int radius;

  public Circle() : this(1)
  {

  }

  public Circle(int radius)
  {
    this.radius = radius;
  }

  public override void build(Builder builder)
  {
    return builder.build(radius);
  }
}
|})

let () = load_test program KeyMap.empty

let program =
  Option.get
    (apply parser
       {|
public class Program
{
  public static void Main()
  {
    Builder builder = new Builder();
    Circle circle = new Circle();

    circle.build(builder);
	}
}

public class Figure 
{
  public virtual void build(Builder builder);
}

public class Circle : Figure
{
  public int radius;

  public Circle() : this(1)
  {

  }

  public Circle(int radius)
  {
    this.radius = radius;
  }

  public override void build(Builder builder)
  {
    return builder.build(radius);
  }
}
|})

let () = load_test program KeyMap.empty

let program =
  Option.get
    (apply parser
       {|
public class Program
{
  public static void Main()
  {
    Builder builder = new Builder();
    Circle circle = new Circle();

    circle.build(builder);
	}
}

public abstract class Figure 
{
  public int radius = 1;

  public abstract void build(Builder builder)
  {
    return builder.build(radius);
  }
}

public class Circle : Figure
{
  public int radius;

  public Circle() : this(1)
  {

  }

  public Circle(int radius)
  {
    this.radius = radius;
  }

  public override void build(Builder builder)
  {
    return builder.build(radius);
  }
}
|})

let () = load_test program KeyMap.empty

let program =
  Option.get
    (apply parser
       {|
public class Program
{
  public static void Main()
  {
    Builder builder = new Builder();
    Circle circle = new Circle();

    circle.build(builder);
	}
}

public abstract class Figure 
{
  public abstract void build(Builder builder);
}

public class Circle : Figure
{
  public int radius;

  public Circle() : this(1)
  {

  }

  public Circle(int radius)
  {
    this.radius = radius;
  }
}
|})

let () = load_test program KeyMap.empty

let () =
  print_string
    "\n-_-_-_-_-_-_-_-_-_-_- Override errors testing -_-_-_-_-_-_-_-_-_-_-\n\n"

let program =
  Option.get
    (apply parser
       {|
public class Program
{
  public static void Main() 
  {
    Person person = new Person(100, 50);
	}
}

public class Person
{
  public int weight;
  public int age;
  
  public Person(int weight, int age) 
  {
    this.weight = weight;
    this.age = age;
  }
  
  public int GetWeight()
  {
    return weight;
  }

  public void SetWeight(int weight)
  {
    this.weight = weight;
  }
  
  public int GetAge()
  {
    return age;
  }

  public void SetAge(int age)
  {
    this.age = age;
  }
}

public class Child : Person
{
  public int cash;
  
  public Child(int weight, int age) : base(weight, age)
  {
    cash = 0;
  }

  public Child(int weight, int age, int cash) : base(weight, age)
  {
    this.cash = cash;
  }
  
  public override int GetCash()
  {
    return cash;
  }
  
  public void SetCash(int cash)
  {
    this.cash = cash;
  }
  
  public void TellEvenNumbers(int count) 
  {
    for (int i = 0; i < count; i++)
    {
      if (i % 2 == 0 && !(i % 2 == 1))
      {
        Console.WriteLine(i);
      }
      else
      {
        continue;
      }
    }
  }
}
|})

let () = load_test program KeyMap.empty

let program =
  Option.get
    (apply parser
       {|
public class Program
{
  public static void Main() 
  {
    Person person = new Person(100, 50);
	}
}

public class Person
{
  public int weight;
  public int age;
  
  public Person(int weight, int age) 
  {
    this.weight = weight;
    this.age = age;
  }
  
  public int GetWeight()
  {
    return weight;
  }

  public void SetWeight(int weight)
  {
    this.weight = weight;
  }
  
  public int GetAge()
  {
    return age;
  }

  public void SetAge(int age)
  {
    this.age = age;
  }
}

public class Child : Person
{
  public int age;
  public int cash;
  
  public Child(int weight, int age) : base(weight, age)
  {
    cash = 0;
  }

  public Child(int weight, int age, int cash) : base(weight, age)
  {
    this.cash = cash;
  }
  
  public int GetCash()
  {
    return cash;
  }
  
  public void SetCash(int cash)
  {
    this.cash = cash;
  }

  public override void SetAge(int age)
  {
    this.age = age + 1;
  }
  
  public void TellEvenNumbers(int count) 
  {
    for (int i = 0; i < count; i++)
    {
      if (i % 2 == 0 && !(i % 2 == 1))
      {
        Console.WriteLine(i);
      }
      else
      {
        continue;
      }
    }
  }
}
|})

let () = load_test program KeyMap.empty
