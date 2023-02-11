(** Copyright 2021-2022, Pavel Alimov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Csharpoop_lib.Parser
open Csharpoop_lib.PrettyPrinter

let parse_result =
  Option.get
    (apply parser
       {|
public class Program
{
  public static void Main() 
  {
    int a=5;
  }
}

public abstract class A
{
  abstract int TestF();
}

public class B :A
{
  public int Pub;

  public B(int temp)
  {
    this.Pub= temp;
  }

  public B()
  {
    this.Pub =1;
  }

  public override int TestF()
  {
    return this.Pub;
  }
}
|})

let () = pretty_print parse_result
