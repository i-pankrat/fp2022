(** Copyright 2021-2022, Pavel Alimov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Csharpoop_lib.Ast
open Csharpoop_lib.Parser

let print_list =
  Format.pp_print_list Format.pp_print_string Format.std_formatter

let () =
  print_string
    "\n-_-_-_-_-_-_-_-_-_-_- Console writeline -_-_-_-_-_-_-_-_-_-_-\n\n"

let parse_result =
  Option.get
    (apply parser
       {|
       public class Program
       {
         public static void Main() 
         {
           int a = 23; 
           Console.WriteLine(23);
           int a = 5;
           Console.WriteLine(a);
         }
       }
|})

let () = print_list (List.map show_objects parse_result)

let () =
  print_string
    "\n-_-_-_-_-_-_-_-_-_-_- Console writeline -_-_-_-_-_-_-_-_-_-_-\n\n"

let parse_result =
  Option.get
    (apply parser
       {|
       public class Program
       {
         public static void Main() 
         {
           Console.WriteLine("aaaaa");
           Console.WriteLine(42);
         }
       }
|})

let () = print_list (List.map show_objects parse_result)

let () =
  print_string
    "\n-_-_-_-_-_-_-_-_-_-_- Assing to method -_-_-_-_-_-_-_-_-_-_-\n\n"

let parse_result =
  Option.get
    (apply parser
       {|
       public class Program
       {
         public static void Main() 
         {
           a.F() = 42;
         }
       }
|})

let () = print_list (List.map show_objects parse_result)

let () =
  print_string
    "\n\
     -_-_-_-_-_-_-_-_-_-_- Cast difficult expression -_-_-_-_-_-_-_-_-_-_-\n\n"

let parse_result =
  Option.get
    (apply parser
       {|
       public class Program
       {
         public static void Main() 
         {
          var a = new A();
          Console.WriteLine("######################################");
          Console.WriteLine(((A) a).F());
         }
       }
|})

let () = print_list (List.map show_objects parse_result)
let () = print_string "\n-_-_-_-_-_-_-_-_-_-_- Cast -_-_-_-_-_-_-_-_-_-_-\n\n"

let parse_result =
  Option.get
    (apply parser
       {|
       public class Program
       {
         public static void Main() 
         {
          int a = (int)23;
          int b = (int)a;
          bool c = (bool)b;
          b = (int)c;
         }
       }
|})

let () = print_list (List.map show_objects parse_result)

let () =
  print_string "\n-_-_-_-_-_-_-_-_-_-_- Cast to class -_-_-_-_-_-_-_-_-_-_-\n\n"

let parse_result =
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
          SomeClass b = (SomeClass)a;
          int c = (int)b.F();
          Console.WriteLine(a.F());
          Console.WriteLine(b.F());
          Console.WriteLine(C.F());
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

let () = print_list (List.map show_objects parse_result)

let () =
  print_string "\n-_-_-_-_-_-_-_-_-_-_- Interface -_-_-_-_-_-_-_-_-_-_-\n\n"

let parse_result =
  Option.get
    (apply parser
       {|
       public class Program
       {
         public static void Main() 
         {
          IInterface a = new SomeClass();
         }
       }

       public interface IInterface
       {

       }

       public class SomeClass : IInterface
       {

       }
|})

let () = print_list (List.map show_objects parse_result)

let () =
  print_string "\n-_-_-_-_-_-_-_-_-_-_- Many parents -_-_-_-_-_-_-_-_-_-_-\n\n"

let parse_result =
  Option.get
    (apply parser
       {|
       public class Program
       {
         public static void Main() 
         {
         }
       }

       public interface IInterface
       {

       }

       public class A
       {

       }

       public class B : IInterface, A
       {

       }

       public class C : IInterface, A
       {

       }
|})

let () = print_list (List.map show_objects parse_result)

let () =
  print_string
    "\n-_-_-_-_-_-_-_-_-_-_- Simple object testing -_-_-_-_-_-_-_-_-_-_-\n\n"

let parse_result =
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

let () = print_list (List.map show_objects parse_result)
