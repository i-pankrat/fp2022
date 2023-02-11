(** Copyright 2021-2022, Pavel Alimov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

val ( ++ ) : Ast.values -> Ast.values -> Ast.values
val ( -- ) : Ast.values -> Ast.values -> Ast.values
val ( ** ) : Ast.values -> Ast.values -> Ast.values
val ( // ) : Ast.values -> Ast.values -> Ast.values
val ( %% ) : Ast.values -> Ast.values -> Ast.values
val ( >>> ) : Ast.values -> Ast.values -> Ast.values
val ( <<< ) : Ast.values -> Ast.values -> Ast.values
val ( >>== ) : Ast.values -> Ast.values -> Ast.values
val ( ==<< ) : Ast.values -> Ast.values -> Ast.values
val ( &&& ) : Ast.values -> Ast.values -> Ast.values
val ( ||| ) : Ast.values -> Ast.values -> Ast.values
val ( !!! ) : Ast.values -> Ast.values
val ( === ) : Ast.values -> Ast.values -> Ast.values
val ( !=! ) : Ast.values -> Ast.values -> Ast.values
