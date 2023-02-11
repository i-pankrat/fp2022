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
        A a = new A();
        Console.WriteLine("######################################");
        Console.WriteLine(((A) a).F());

        B b = new B();
        Console.WriteLine("######################################");
        Console.WriteLine(((A) b).F());
        Console.WriteLine(((IInterface) b).F());

        C c = new C();
        Console.WriteLine("######################################");
        Console.WriteLine(((A) c).F());
        Console.WriteLine(((IInterface) c).F());

        D d = new D();
        Console.WriteLine("######################################");
        Console.WriteLine(((A) d).F());
        Console.WriteLine(d.F());
        Console.WriteLine(((IInterface) d).F());

        D1 d1 = new D1();
        Console.WriteLine("######################################");
        Console.WriteLine(((IInterface) d1).F());
        
        E e = new E();
        Console.WriteLine("######################################");
        Console.WriteLine(((A) e).F());
        Console.WriteLine(e.F());
        Console.WriteLine(((IInterface) e).F());
        Console.WriteLine(((IInterface) ((B) e)).F());

        G g = new G();
        Console.WriteLine("######################################");
        Console.WriteLine(((A) g).F());
        Console.WriteLine(((IInterface) g).F());
        Console.WriteLine(((E) g).F());
      }
    }

    public interface IInterface {
        int F();
    }
    
    public class A {
        public virtual int F() {
            return 7;
        }
    }
    
    public class B : A, IInterface {
        public override int F() {
            return 8;
        }
    }
    
    public class C : B, IInterface {
        public override int F() {
            return 9;
        }
    
        public int IInterface.F() {
            return 100;
        }
    }    
    public class D : C {
        public new int F() {
            return -10;
        }
    }
    public class D1 : C, IInterface {
        public new int F() {
            return -11;
        }
    }
    public class E : C {
        public new virtual int F() {
            return 10;
        }
    }
    
    public class G : E, IInterface {
        public override int F() {
            return 11;
        }
    }
|})

let () = print_list (List.map show_objects parse_result)
