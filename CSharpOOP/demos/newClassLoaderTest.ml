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
    "-_-_-_-_-_-_-_-_-_-_- Program with new testing -_-_-_-_-_-_-_-_-_-_-\n\n"

let program =
  Option.get
    (apply parser
       {|
public class Program
    {
        public static void Main()
        {
            A a = new A();
            Console.WriteLine("######################################");
            Console.WriteLine("a to A .F() = " + ((A)a).F());       

            B b = new B();
            Console.WriteLine("######################################");
            Console.WriteLine("b.F() = " + b.F());            
            Console.WriteLine("b to A .F() = " + ((A)b).F());      
            Console.WriteLine("b to I .F() = " + ((IInterface)b).F());  

            C c = new C();
            Console.WriteLine("######################################");
            Console.WriteLine("c.F() = " + c.F());          
            Console.WriteLine("c to A .F() = " + ((A)c).F()); 
            Console.WriteLine("c to A .F() = " + ((B)c).F());        
            Console.WriteLine("c to I .F() = " + ((IInterface)c).F()); 

            D d = new D();
            Console.WriteLine("######################################");
            Console.WriteLine("d.F() = " + d.F());         
            Console.WriteLine("d to A .F() = " + ((A)d).F());   
            Console.WriteLine("d to B .F() = " + ((B)d).F());   
            Console.WriteLine("d to C .F() = " + ((C)d).F());     
            Console.WriteLine("d to I .F() = " + ((IInterface)d).F()); 

            D1 d1 = new D1();
            Console.WriteLine("######################################");
            Console.WriteLine("d1.F() = " + d1.F());     
            Console.WriteLine("d1 to A .F() = " + ((A)d1).F());
            Console.WriteLine("d1 to B .F() = " + ((B)d1).F()); 
            Console.WriteLine("d1 to C .F() = " + ((C)d1).F());
            Console.WriteLine("d1 to I .F() = " + ((IInterface)d1).F());

            E e = new E();
            Console.WriteLine("######################################");
            Console.WriteLine("e.F() = " + e.F());
            Console.WriteLine("e to A .F() = " + ((A)e).F()); 
            Console.WriteLine("e to B .F() = " + ((B)e).F());
            Console.WriteLine("e to C .F() = " + ((C)e).F()); 
            Console.WriteLine("e to I .F() = " + ((IInterface)e).F());
            Console.WriteLine("e to A to I .F() = " + ((IInterface)((A)e)).F()); 
            Console.WriteLine("e to B to I .F() = " + ((IInterface)((B)e)).F());
            Console.WriteLine("e to C to I .F() = " + ((IInterface)((C)e)).F()); 
            Console.WriteLine("e to B to A .F() = " + ((A)((B)e)).F()); 
            Console.WriteLine("e to C to B .F() = " + ((B)((C)e)).F()); 
            Console.WriteLine("e to C to A .F() = " + ((A)((C)e)).F());

            G g = new G();
            Console.WriteLine("######################################");
            Console.WriteLine("g.F() = " + g.F()); 
            Console.WriteLine("g to A .F() = " + ((A)g).F()); 
            Console.WriteLine("g to B .F() = " + ((B)g).F());
            Console.WriteLine("g to C .F() = " + ((C)g).F());  
            Console.WriteLine("g to E .F() = " + ((E)g).F());  
            Console.WriteLine("g to I .F() = " + ((IInterface)g).F());
            
            H h = new H();
            Console.WriteLine("######################################");
            Console.WriteLine("h.F() = " + h.F()); 
            Console.WriteLine("h to A .F() = " + ((A)h).F()); 
            Console.WriteLine("h to B .F() = " + ((B)h).F()); 
            Console.WriteLine("h to C .F() = " + ((C)h).F()); 
            Console.WriteLine("h to E .F() = " + ((E)h).F()); 
            Console.WriteLine("h to I .F() = " + ((IInterface)h).F());
        }
    }

    public interface IInterface
    {
        int F();
    }

    public class A
    {
        public virtual int F()
        {
            return 7;
        }
    }

    public class B : A, IInterface
    {
        public override int F()
        {
            return 8;
        }
    }

    public class C : B, IInterface
    {
        public override int F()
        {
            return 9;
        }

        public int IInterface.F()
        {
            return 100;
        }
    }
    public class D : C
    {
        public new int F()
        {
            return -10;
        }
    }
    public class D1 : C, IInterface
    {
        public new int F()
        {
            return -11;
        }
    }
    public class E : C
    {
        public new virtual int F()
        {
            return 10;
        }
    }

    public class H : E
    {
        public override int F()
        {
            return 19;
        }
    }

    public class G : E, IInterface
    {
        public override int F()
        {
            return 11;
        }
    }
|})

let () = load_test program KeyMap.empty
