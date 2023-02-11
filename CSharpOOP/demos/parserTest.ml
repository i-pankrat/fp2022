(** Copyright 2021-2022, Pavel Alimov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Csharpoop_lib.Ast
open Csharpoop_lib.Parser

let print_list =
  Format.pp_print_list Format.pp_print_string Format.std_formatter

let parse_result =
  Option.get
    (apply parser
       {|
public class Program
{
  public static void Main() 
  {
    Person person = new Person(100, 50);

    person.SetAge(45);
	  Console.WriteLine(person.GetAge());
		
    Child child = new Child(50, 10);
    
    child.SetCash(1000);
    Console.WriteLine(child.GetCash());

		child.TellEvenNumbers(333);
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

  public Child(int weight, int age, int cash) : this(weight, age)
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

let () = print_list (List.map show_objects parse_result)
