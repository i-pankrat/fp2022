  $ (cd ../../../../default && demos/newClassLoaderTest.exe)
  -_-_-_-_-_-_-_-_-_-_- Program with new testing -_-_-_-_-_-_-_-_-_-_-
  
  ["A": { this_key = "A"; fields_table = [];
          methods_table =
          ["EqualsTRef ("Object")": { method_type = TInt;
                                      method_modifiers = [Virtual; Public];
                                      arguments =
                                      [(TRef ("Object"), Name ("obj"))];
                                      key = "EqualsTRef (\"Object\")";
                                      interface_key = None;
                                      body =
                                      Some (StatementBlock ([If
                                                             (Equal
                                                              (This,
                                                               Identifier ("obj")),
                                                              Return (Some (
                                                                      Value (
                                                                      VInt (1)))),
                                                              Some (Return (
                                                                     Some (
                                                                      Value (
                                                                      VInt (0))))))]));
                                      is_overriden = false },
           "F": { method_type = TInt; method_modifiers = [Public; Virtual];
                  arguments = []; key = "F"; interface_key = None;
                  body =
                  Some (StatementBlock ([Return (Some (Value (VInt (7))))]));
                  is_overriden = false },
           "ToString": { method_type = TString;
                         method_modifiers = [Virtual; Public]; arguments = [];
                         key = "ToString"; interface_key = None;
                         body =
                         Some (StatementBlock ([Return (Some (Value (VString ("Object"))))]));
                         is_overriden = false },
           ];
          constructors_table =
          ["A": { key = "A"; arguments = []; call_constructor = None;
                  body = StatementBlock ([]) },
           ];
          children_keys = ["B"]; is_abstract = false; is_static = false;
          is_class = true; parent_key = ["Object"];
          decl_tree =
          Class
          ([Public], Name ("A"), [],
           [([Public; Virtual],
             Method
             (TInt, Name ("F"), [],
              Some (StatementBlock ([Return (Some (Value (VInt (7))))]))))])
          },
   "B": { this_key = "B"; fields_table = [];
          methods_table =
          ["EqualsTRef ("Object")": { method_type = TInt;
                                      method_modifiers = [Virtual; Public];
                                      arguments =
                                      [(TRef ("Object"), Name ("obj"))];
                                      key = "EqualsTRef (\"Object\")";
                                      interface_key = None;
                                      body =
                                      Some (StatementBlock ([If
                                                             (Equal
                                                              (This,
                                                               Identifier ("obj")),
                                                              Return (Some (
                                                                      Value (
                                                                      VInt (1)))),
                                                              Some (Return (
                                                                     Some (
                                                                      Value (
                                                                      VInt (0))))))]));
                                      is_overriden = false },
           "F": { method_type = TInt; method_modifiers = [Public; Override];
                  arguments = []; key = "F"; interface_key = None;
                  body =
                  Some (StatementBlock ([Return (Some (Value (VInt (8))))]));
                  is_overriden = true },
           "ToString": { method_type = TString;
                         method_modifiers = [Virtual; Public]; arguments = [];
                         key = "ToString"; interface_key = None;
                         body =
                         Some (StatementBlock ([Return (Some (Value (VString ("Object"))))]));
                         is_overriden = false },
           ];
          constructors_table =
          ["B": { key = "B"; arguments = []; call_constructor = None;
                  body = StatementBlock ([]) },
           ];
          children_keys = ["C"]; is_abstract = false; is_static = false;
          is_class = true; parent_key = ["A"; "IInterface"];
          decl_tree =
          Class
          ([Public], Name ("B"), [Name ("A"); Name ("IInterface")],
           [([Public; Override],
             Method
             (TInt, Name ("F"), [],
              Some (StatementBlock ([Return (Some (Value (VInt (8))))]))))])
          },
   "C": { this_key = "C"; fields_table = [];
          methods_table =
          ["EqualsTRef ("Object")": { method_type = TInt;
                                      method_modifiers = [Virtual; Public];
                                      arguments =
                                      [(TRef ("Object"), Name ("obj"))];
                                      key = "EqualsTRef (\"Object\")";
                                      interface_key = None;
                                      body =
                                      Some (StatementBlock ([If
                                                             (Equal
                                                              (This,
                                                               Identifier ("obj")),
                                                              Return (Some (
                                                                      Value (
                                                                      VInt (1)))),
                                                              Some (Return (
                                                                     Some (
                                                                      Value (
                                                                      VInt (0))))))]));
                                      is_overriden = false },
           "F": { method_type = TInt; method_modifiers = [Public; Override];
                  arguments = []; key = "F"; interface_key = None;
                  body =
                  Some (StatementBlock ([Return (Some (Value (VInt (9))))]));
                  is_overriden = true },
           "IInterface.F": { method_type = TInt; method_modifiers = [Public];
                             arguments = []; key = "IInterface.F";
                             interface_key = Some ("IInterface");
                             body =
                             Some (StatementBlock ([Return (Some (Value (
                                                                   VInt (100))))]));
                             is_overriden = true },
           "ToString": { method_type = TString;
                         method_modifiers = [Virtual; Public]; arguments = [];
                         key = "ToString"; interface_key = None;
                         body =
                         Some (StatementBlock ([Return (Some (Value (VString ("Object"))))]));
                         is_overriden = false },
           ];
          constructors_table =
          ["C": { key = "C"; arguments = []; call_constructor = None;
                  body = StatementBlock ([]) },
           ];
          children_keys = ["E"; "D1"; "D"]; is_abstract = false;
          is_static = false; is_class = true; parent_key = ["B"; "IInterface"];
          decl_tree =
          Class
          ([Public], Name ("C"), [Name ("B"); Name ("IInterface")],
           [([Public; Override],
             Method
             (TInt, Name ("F"), [],
              Some (StatementBlock ([Return (Some (Value (VInt (9))))]))));
            ([Public],
             Method
             (TInt, Name ("IInterface.F"), [],
              Some (StatementBlock ([Return (Some (Value (VInt (100))))]))))])
          },
   "D": { this_key = "D"; fields_table = [];
          methods_table =
          ["EqualsTRef ("Object")": { method_type = TInt;
                                      method_modifiers = [Virtual; Public];
                                      arguments =
                                      [(TRef ("Object"), Name ("obj"))];
                                      key = "EqualsTRef (\"Object\")";
                                      interface_key = None;
                                      body =
                                      Some (StatementBlock ([If
                                                             (Equal
                                                              (This,
                                                               Identifier ("obj")),
                                                              Return (Some (
                                                                      Value (
                                                                      VInt (1)))),
                                                              Some (Return (
                                                                     Some (
                                                                      Value (
                                                                      VInt (0))))))]));
                                      is_overriden = false },
           "F": { method_type = TInt; method_modifiers = [Public; New];
                  arguments = []; key = "F"; interface_key = None;
                  body =
                  Some (StatementBlock ([Return (Some (Sub
                                                       (Value (VInt (0)),
                                                        Value (VInt (10)))))]));
                  is_overriden = true },
           "ToString": { method_type = TString;
                         method_modifiers = [Virtual; Public]; arguments = [];
                         key = "ToString"; interface_key = None;
                         body =
                         Some (StatementBlock ([Return (Some (Value (VString ("Object"))))]));
                         is_overriden = false },
           ];
          constructors_table =
          ["D": { key = "D"; arguments = []; call_constructor = None;
                  body = StatementBlock ([]) },
           ];
          children_keys = []; is_abstract = false; is_static = false;
          is_class = true; parent_key = ["C"];
          decl_tree =
          Class
          ([Public], Name ("D"), [Name ("C")],
           [([Public; New],
             Method
             (TInt, Name ("F"), [],
              Some (StatementBlock ([Return (Some (Sub
                                                   (Value (VInt (0)),
                                                    Value (VInt (10)))))]))))])
          },
   "D1": { this_key = "D1"; fields_table = [];
           methods_table =
           ["EqualsTRef ("Object")": { method_type = TInt;
                                       method_modifiers = [Virtual; Public];
                                       arguments =
                                       [(TRef ("Object"), Name ("obj"))];
                                       key = "EqualsTRef (\"Object\")";
                                       interface_key = None;
                                       body =
                                       Some (StatementBlock ([If
                                                              (Equal
                                                               (This,
                                                                Identifier ("obj")),
                                                               Return (
                                                                Some (Value (
                                                                      VInt (1)))),
                                                               Some (Return (
                                                                      Some (
                                                                      Value (
                                                                      VInt (0))))))]));
                                       is_overriden = false },
            "F": { method_type = TInt; method_modifiers = [Public; New];
                   arguments = []; key = "F"; interface_key = None;
                   body =
                   Some (StatementBlock ([Return (Some (Sub
                                                        (Value (VInt (0)),
                                                         Value (VInt (11)))))]));
                   is_overriden = true },
            "ToString": { method_type = TString;
                          method_modifiers = [Virtual; Public]; arguments = [];
                          key = "ToString"; interface_key = None;
                          body =
                          Some (StatementBlock ([Return (Some (Value (VString ("Object"))))]));
                          is_overriden = false },
            ];
           constructors_table =
           ["D1": { key = "D1"; arguments = []; call_constructor = None;
                    body = StatementBlock ([]) },
            ];
           children_keys = []; is_abstract = false; is_static = false;
           is_class = true; parent_key = ["C"; "IInterface"];
           decl_tree =
           Class
           ([Public], Name ("D1"), [Name ("C"); Name ("IInterface")],
            [([Public; New],
              Method
              (TInt, Name ("F"), [],
               Some (StatementBlock ([Return (Some (Sub
                                                    (Value (VInt (0)),
                                                     Value (VInt (11)))))]))))])
           },
   "E": { this_key = "E"; fields_table = [];
          methods_table =
          ["EqualsTRef ("Object")": { method_type = TInt;
                                      method_modifiers = [Virtual; Public];
                                      arguments =
                                      [(TRef ("Object"), Name ("obj"))];
                                      key = "EqualsTRef (\"Object\")";
                                      interface_key = None;
                                      body =
                                      Some (StatementBlock ([If
                                                             (Equal
                                                              (This,
                                                               Identifier ("obj")),
                                                              Return (Some (
                                                                      Value (
                                                                      VInt (1)))),
                                                              Some (Return (
                                                                     Some (
                                                                      Value (
                                                                      VInt (0))))))]));
                                      is_overriden = false },
           "F": { method_type = TInt;
                  method_modifiers = [Public; New; Virtual]; arguments = [];
                  key = "F"; interface_key = None;
                  body =
                  Some (StatementBlock ([Return (Some (Value (VInt (10))))]));
                  is_overriden = true },
           "ToString": { method_type = TString;
                         method_modifiers = [Virtual; Public]; arguments = [];
                         key = "ToString"; interface_key = None;
                         body =
                         Some (StatementBlock ([Return (Some (Value (VString ("Object"))))]));
                         is_overriden = false },
           ];
          constructors_table =
          ["E": { key = "E"; arguments = []; call_constructor = None;
                  body = StatementBlock ([]) },
           ];
          children_keys = ["H"; "G"]; is_abstract = false; is_static = false;
          is_class = true; parent_key = ["C"];
          decl_tree =
          Class
          ([Public], Name ("E"), [Name ("C")],
           [([Public; New; Virtual],
             Method
             (TInt, Name ("F"), [],
              Some (StatementBlock ([Return (Some (Value (VInt (10))))]))))])
          },
   "G": { this_key = "G"; fields_table = [];
          methods_table =
          ["EqualsTRef ("Object")": { method_type = TInt;
                                      method_modifiers = [Virtual; Public];
                                      arguments =
                                      [(TRef ("Object"), Name ("obj"))];
                                      key = "EqualsTRef (\"Object\")";
                                      interface_key = None;
                                      body =
                                      Some (StatementBlock ([If
                                                             (Equal
                                                              (This,
                                                               Identifier ("obj")),
                                                              Return (Some (
                                                                      Value (
                                                                      VInt (1)))),
                                                              Some (Return (
                                                                     Some (
                                                                      Value (
                                                                      VInt (0))))))]));
                                      is_overriden = false },
           "F": { method_type = TInt; method_modifiers = [Public; Override];
                  arguments = []; key = "F"; interface_key = None;
                  body =
                  Some (StatementBlock ([Return (Some (Value (VInt (11))))]));
                  is_overriden = true },
           "ToString": { method_type = TString;
                         method_modifiers = [Virtual; Public]; arguments = [];
                         key = "ToString"; interface_key = None;
                         body =
                         Some (StatementBlock ([Return (Some (Value (VString ("Object"))))]));
                         is_overriden = false },
           ];
          constructors_table =
          ["G": { key = "G"; arguments = []; call_constructor = None;
                  body = StatementBlock ([]) },
           ];
          children_keys = []; is_abstract = false; is_static = false;
          is_class = true; parent_key = ["E"; "IInterface"];
          decl_tree =
          Class
          ([Public], Name ("G"), [Name ("E"); Name ("IInterface")],
           [([Public; Override],
             Method
             (TInt, Name ("F"), [],
              Some (StatementBlock ([Return (Some (Value (VInt (11))))]))))])
          },
   "H": { this_key = "H"; fields_table = [];
          methods_table =
          ["EqualsTRef ("Object")": { method_type = TInt;
                                      method_modifiers = [Virtual; Public];
                                      arguments =
                                      [(TRef ("Object"), Name ("obj"))];
                                      key = "EqualsTRef (\"Object\")";
                                      interface_key = None;
                                      body =
                                      Some (StatementBlock ([If
                                                             (Equal
                                                              (This,
                                                               Identifier ("obj")),
                                                              Return (Some (
                                                                      Value (
                                                                      VInt (1)))),
                                                              Some (Return (
                                                                     Some (
                                                                      Value (
                                                                      VInt (0))))))]));
                                      is_overriden = false },
           "F": { method_type = TInt; method_modifiers = [Public; Override];
                  arguments = []; key = "F"; interface_key = None;
                  body =
                  Some (StatementBlock ([Return (Some (Value (VInt (19))))]));
                  is_overriden = true },
           "ToString": { method_type = TString;
                         method_modifiers = [Virtual; Public]; arguments = [];
                         key = "ToString"; interface_key = None;
                         body =
                         Some (StatementBlock ([Return (Some (Value (VString ("Object"))))]));
                         is_overriden = false },
           ];
          constructors_table =
          ["H": { key = "H"; arguments = []; call_constructor = None;
                  body = StatementBlock ([]) },
           ];
          children_keys = []; is_abstract = false; is_static = false;
          is_class = true; parent_key = ["E"];
          decl_tree =
          Class
          ([Public], Name ("H"), [Name ("E")],
           [([Public; Override],
             Method
             (TInt, Name ("F"), [],
              Some (StatementBlock ([Return (Some (Value (VInt (19))))]))))])
          },
   "IInterface": { this_key = "IInterface"; fields_table = [];
                   methods_table =
                   ["F": { method_type = TInt;
                           method_modifiers = [Abstract; Virtual; Public];
                           arguments = []; key = "F"; interface_key = None;
                           body = None; is_overriden = false },
                    ];
                   constructors_table = [];
                   children_keys = ["G"; "D1"; "C"; "B"]; is_abstract = false;
                   is_static = false; is_class = false;
                   parent_key = ["Object"];
                   decl_tree =
                   Interface
                   (Public, Name ("IInterface"), [],
                    [([Public], Method (TInt, Name ("F"), [], None))])
                   },
   "Object": { this_key = "Object"; fields_table = [];
               methods_table =
               ["EqualsTRef ("Object")": { method_type = TInt;
                                           method_modifiers = [Virtual; Public];
                                           arguments =
                                           [(TRef ("Object"), Name ("obj"))];
                                           key = "EqualsTRef (\"Object\")";
                                           interface_key = None;
                                           body =
                                           Some (StatementBlock ([If
                                                                  (Equal
                                                                   (This,
                                                                    Identifier ("obj")),
                                                                   Return (
                                                                    Some (
                                                                     Value (
                                                                      VInt (1)))),
                                                                   Some (
                                                                    Return (
                                                                     Some (
                                                                      Value (
                                                                      VInt (0))))))]));
                                           is_overriden = false },
                "ToString": { method_type = TString;
                              method_modifiers = [Virtual; Public];
                              arguments = []; key = "ToString";
                              interface_key = None;
                              body =
                              Some (StatementBlock ([Return (Some (Value (
                                                                    VString ("Object"))))]));
                              is_overriden = false },
                ];
               constructors_table =
               ["Object": { key = "Object"; arguments = [];
                            call_constructor = None; body = StatementBlock ([])
                            },
                ];
               children_keys = ["Program"; "IInterface"; "A"];
               is_abstract = false; is_static = false; is_class = true;
               parent_key = [];
               decl_tree =
               Class
               ([Public], Name ("Object"), [],
                [([Public],
                  Method
                  (TInt, Name ("Equals"), [(TRef ("Object"), Name ("obj"))],
                   Some (StatementBlock ([If
                                          (Equal (This, Identifier ("obj")),
                                           Return (Some (Value (VInt (1)))),
                                           Some (Return (Some (Value (VInt (0))))))]))));
                 ([Public],
                  Method
                  (TString, Name ("ToString"), [],
                   Some (StatementBlock ([Return (Some (Value (VString ("Object"))))]))))])
               },
   "Program": { this_key = "Program"; fields_table = [];
                methods_table =
                ["EqualsTRef ("Object")": { method_type = TInt;
                                            method_modifiers =
                                            [Virtual; Public];
                                            arguments =
                                            [(TRef ("Object"), Name ("obj"))];
                                            key = "EqualsTRef (\"Object\")";
                                            interface_key = None;
                                            body =
                                            Some (StatementBlock ([If
                                                                   (Equal
                                                                    (This,
                                                                     Identifier ("obj")),
                                                                    Return (
                                                                     Some (
                                                                      Value (
                                                                      VInt (1)))),
                                                                    Some (
                                                                     Return (
                                                                      Some (
                                                                      Value (
                                                                      VInt (0))))))]));
                                            is_overriden = false },
                 "Main": { method_type = TVoid;
                           method_modifiers = [Public; Static]; arguments = [];
                           key = "Main"; interface_key = None;
                           body =
                           Some (StatementBlock ([VariableDecl
                                                  (None, TRef ("A"),
                                                   [(Name ("a"),
                                                     Some (ClassCreation
                                                           (Name ("A"), [])))]);
                                                  Print (Value (VString ("######################################")));
                                                  Print (Add
                                                         (Value (VString ("a to A .F() = ")),
                                                          AccessByPoint
                                                          (Cast
                                                           (TRef ("A"),
                                                            Identifier ("a")),
                                                           CallMethod
                                                           (Identifier ("F"),
                                                            []))));
                                                  VariableDecl
                                                  (None, TRef ("B"),
                                                   [(Name ("b"),
                                                     Some (ClassCreation
                                                           (Name ("B"), [])))]);
                                                  Print (Value (VString ("######################################")));
                                                  Print (Add
                                                         (Value (VString ("b.F() = ")),
                                                          AccessByPoint
                                                          (Identifier ("b"),
                                                           CallMethod
                                                           (Identifier ("F"),
                                                            []))));
                                                  Print (Add
                                                         (Value (VString ("b to A .F() = ")),
                                                          AccessByPoint
                                                          (Cast
                                                           (TRef ("A"),
                                                            Identifier ("b")),
                                                           CallMethod
                                                           (Identifier ("F"),
                                                            []))));
                                                  Print (Add
                                                         (Value (VString ("b to I .F() = ")),
                                                          AccessByPoint
                                                          (Cast
                                                           (TRef ("IInterface"),
                                                            Identifier ("b")),
                                                           CallMethod
                                                           (Identifier ("F"),
                                                            []))));
                                                  VariableDecl
                                                  (None, TRef ("C"),
                                                   [(Name ("c"),
                                                     Some (ClassCreation
                                                           (Name ("C"), [])))]);
                                                  Print (Value (VString ("######################################")));
                                                  Print (Add
                                                         (Value (VString ("c.F() = ")),
                                                          AccessByPoint
                                                          (Identifier ("c"),
                                                           CallMethod
                                                           (Identifier ("F"),
                                                            []))));
                                                  Print (Add
                                                         (Value (VString ("c to A .F() = ")),
                                                          AccessByPoint
                                                          (Cast
                                                           (TRef ("A"),
                                                            Identifier ("c")),
                                                           CallMethod
                                                           (Identifier ("F"),
                                                            []))));
                                                  Print (Add
                                                         (Value (VString ("c to A .F() = ")),
                                                          AccessByPoint
                                                          (Cast
                                                           (TRef ("B"),
                                                            Identifier ("c")),
                                                           CallMethod
                                                           (Identifier ("F"),
                                                            []))));
                                                  Print (Add
                                                         (Value (VString ("c to I .F() = ")),
                                                          AccessByPoint
                                                          (Cast
                                                           (TRef ("IInterface"),
                                                            Identifier ("c")),
                                                           CallMethod
                                                           (Identifier ("F"),
                                                            []))));
                                                  VariableDecl
                                                  (None, TRef ("D"),
                                                   [(Name ("d"),
                                                     Some (ClassCreation
                                                           (Name ("D"), [])))]);
                                                  Print (Value (VString ("######################################")));
                                                  Print (Add
                                                         (Value (VString ("d.F() = ")),
                                                          AccessByPoint
                                                          (Identifier ("d"),
                                                           CallMethod
                                                           (Identifier ("F"),
                                                            []))));
                                                  Print (Add
                                                         (Value (VString ("d to A .F() = ")),
                                                          AccessByPoint
                                                          (Cast
                                                           (TRef ("A"),
                                                            Identifier ("d")),
                                                           CallMethod
                                                           (Identifier ("F"),
                                                            []))));
                                                  Print (Add
                                                         (Value (VString ("d to B .F() = ")),
                                                          AccessByPoint
                                                          (Cast
                                                           (TRef ("B"),
                                                            Identifier ("d")),
                                                           CallMethod
                                                           (Identifier ("F"),
                                                            []))));
                                                  Print (Add
                                                         (Value (VString ("d to C .F() = ")),
                                                          AccessByPoint
                                                          (Cast
                                                           (TRef ("C"),
                                                            Identifier ("d")),
                                                           CallMethod
                                                           (Identifier ("F"),
                                                            []))));
                                                  Print (Add
                                                         (Value (VString ("d to I .F() = ")),
                                                          AccessByPoint
                                                          (Cast
                                                           (TRef ("IInterface"),
                                                            Identifier ("d")),
                                                           CallMethod
                                                           (Identifier ("F"),
                                                            []))));
                                                  VariableDecl
                                                  (None, TRef ("D1"),
                                                   [(Name ("d1"),
                                                     Some (ClassCreation
                                                           (Name ("D1"), [])))]);
                                                  Print (Value (VString ("######################################")));
                                                  Print (Add
                                                         (Value (VString ("d1.F() = ")),
                                                          AccessByPoint
                                                          (Identifier ("d1"),
                                                           CallMethod
                                                           (Identifier ("F"),
                                                            []))));
                                                  Print (Add
                                                         (Value (VString ("d1 to A .F() = ")),
                                                          AccessByPoint
                                                          (Cast
                                                           (TRef ("A"),
                                                            Identifier ("d1")),
                                                           CallMethod
                                                           (Identifier ("F"),
                                                            []))));
                                                  Print (Add
                                                         (Value (VString ("d1 to B .F() = ")),
                                                          AccessByPoint
                                                          (Cast
                                                           (TRef ("B"),
                                                            Identifier ("d1")),
                                                           CallMethod
                                                           (Identifier ("F"),
                                                            []))));
                                                  Print (Add
                                                         (Value (VString ("d1 to C .F() = ")),
                                                          AccessByPoint
                                                          (Cast
                                                           (TRef ("C"),
                                                            Identifier ("d1")),
                                                           CallMethod
                                                           (Identifier ("F"),
                                                            []))));
                                                  Print (Add
                                                         (Value (VString ("d1 to I .F() = ")),
                                                          AccessByPoint
                                                          (Cast
                                                           (TRef ("IInterface"),
                                                            Identifier ("d1")),
                                                           CallMethod
                                                           (Identifier ("F"),
                                                            []))));
                                                  VariableDecl
                                                  (None, TRef ("E"),
                                                   [(Name ("e"),
                                                     Some (ClassCreation
                                                           (Name ("E"), [])))]);
                                                  Print (Value (VString ("######################################")));
                                                  Print (Add
                                                         (Value (VString ("e.F() = ")),
                                                          AccessByPoint
                                                          (Identifier ("e"),
                                                           CallMethod
                                                           (Identifier ("F"),
                                                            []))));
                                                  Print (Add
                                                         (Value (VString ("e to A .F() = ")),
                                                          AccessByPoint
                                                          (Cast
                                                           (TRef ("A"),
                                                            Identifier ("e")),
                                                           CallMethod
                                                           (Identifier ("F"),
                                                            []))));
                                                  Print (Add
                                                         (Value (VString ("e to B .F() = ")),
                                                          AccessByPoint
                                                          (Cast
                                                           (TRef ("B"),
                                                            Identifier ("e")),
                                                           CallMethod
                                                           (Identifier ("F"),
                                                            []))));
                                                  Print (Add
                                                         (Value (VString ("e to C .F() = ")),
                                                          AccessByPoint
                                                          (Cast
                                                           (TRef ("C"),
                                                            Identifier ("e")),
                                                           CallMethod
                                                           (Identifier ("F"),
                                                            []))));
                                                  Print (Add
                                                         (Value (VString ("e to I .F() = ")),
                                                          AccessByPoint
                                                          (Cast
                                                           (TRef ("IInterface"),
                                                            Identifier ("e")),
                                                           CallMethod
                                                           (Identifier ("F"),
                                                            []))));
                                                  Print (Add
                                                         (Value (VString ("e to A to I .F() = ")),
                                                          AccessByPoint
                                                          (Cast
                                                           (TRef ("IInterface"),
                                                            Cast
                                                            (TRef ("A"),
                                                             Identifier ("e"))),
                                                           CallMethod
                                                           (Identifier ("F"),
                                                            []))));
                                                  Print (Add
                                                         (Value (VString ("e to B to I .F() = ")),
                                                          AccessByPoint
                                                          (Cast
                                                           (TRef ("IInterface"),
                                                            Cast
                                                            (TRef ("B"),
                                                             Identifier ("e"))),
                                                           CallMethod
                                                           (Identifier ("F"),
                                                            []))));
                                                  Print (Add
                                                         (Value (VString ("e to C to I .F() = ")),
                                                          AccessByPoint
                                                          (Cast
                                                           (TRef ("IInterface"),
                                                            Cast
                                                            (TRef ("C"),
                                                             Identifier ("e"))),
                                                           CallMethod
                                                           (Identifier ("F"),
                                                            []))));
                                                  Print (Add
                                                         (Value (VString ("e to B to A .F() = ")),
                                                          AccessByPoint
                                                          (Cast
                                                           (TRef ("A"),
                                                            Cast
                                                            (TRef ("B"),
                                                             Identifier ("e"))),
                                                           CallMethod
                                                           (Identifier ("F"),
                                                            []))));
                                                  Print (Add
                                                         (Value (VString ("e to C to B .F() = ")),
                                                          AccessByPoint
                                                          (Cast
                                                           (TRef ("B"),
                                                            Cast
                                                            (TRef ("C"),
                                                             Identifier ("e"))),
                                                           CallMethod
                                                           (Identifier ("F"),
                                                            []))));
                                                  Print (Add
                                                         (Value (VString ("e to C to A .F() = ")),
                                                          AccessByPoint
                                                          (Cast
                                                           (TRef ("A"),
                                                            Cast
                                                            (TRef ("C"),
                                                             Identifier ("e"))),
                                                           CallMethod
                                                           (Identifier ("F"),
                                                            []))));
                                                  VariableDecl
                                                  (None, TRef ("G"),
                                                   [(Name ("g"),
                                                     Some (ClassCreation
                                                           (Name ("G"), [])))]);
                                                  Print (Value (VString ("######################################")));
                                                  Print (Add
                                                         (Value (VString ("g.F() = ")),
                                                          AccessByPoint
                                                          (Identifier ("g"),
                                                           CallMethod
                                                           (Identifier ("F"),
                                                            []))));
                                                  Print (Add
                                                         (Value (VString ("g to A .F() = ")),
                                                          AccessByPoint
                                                          (Cast
                                                           (TRef ("A"),
                                                            Identifier ("g")),
                                                           CallMethod
                                                           (Identifier ("F"),
                                                            []))));
                                                  Print (Add
                                                         (Value (VString ("g to B .F() = ")),
                                                          AccessByPoint
                                                          (Cast
                                                           (TRef ("B"),
                                                            Identifier ("g")),
                                                           CallMethod
                                                           (Identifier ("F"),
                                                            []))));
                                                  Print (Add
                                                         (Value (VString ("g to C .F() = ")),
                                                          AccessByPoint
                                                          (Cast
                                                           (TRef ("C"),
                                                            Identifier ("g")),
                                                           CallMethod
                                                           (Identifier ("F"),
                                                            []))));
                                                  Print (Add
                                                         (Value (VString ("g to E .F() = ")),
                                                          AccessByPoint
                                                          (Cast
                                                           (TRef ("E"),
                                                            Identifier ("g")),
                                                           CallMethod
                                                           (Identifier ("F"),
                                                            []))));
                                                  Print (Add
                                                         (Value (VString ("g to I .F() = ")),
                                                          AccessByPoint
                                                          (Cast
                                                           (TRef ("IInterface"),
                                                            Identifier ("g")),
                                                           CallMethod
                                                           (Identifier ("F"),
                                                            []))));
                                                  VariableDecl
                                                  (None, TRef ("H"),
                                                   [(Name ("h"),
                                                     Some (ClassCreation
                                                           (Name ("H"), [])))]);
                                                  Print (Value (VString ("######################################")));
                                                  Print (Add
                                                         (Value (VString ("h.F() = ")),
                                                          AccessByPoint
                                                          (Identifier ("h"),
                                                           CallMethod
                                                           (Identifier ("F"),
                                                            []))));
                                                  Print (Add
                                                         (Value (VString ("h to A .F() = ")),
                                                          AccessByPoint
                                                          (Cast
                                                           (TRef ("A"),
                                                            Identifier ("h")),
                                                           CallMethod
                                                           (Identifier ("F"),
                                                            []))));
                                                  Print (Add
                                                         (Value (VString ("h to B .F() = ")),
                                                          AccessByPoint
                                                          (Cast
                                                           (TRef ("B"),
                                                            Identifier ("h")),
                                                           CallMethod
                                                           (Identifier ("F"),
                                                            []))));
                                                  Print (Add
                                                         (Value (VString ("h to C .F() = ")),
                                                          AccessByPoint
                                                          (Cast
                                                           (TRef ("C"),
                                                            Identifier ("h")),
                                                           CallMethod
                                                           (Identifier ("F"),
                                                            []))));
                                                  Print (Add
                                                         (Value (VString ("h to E .F() = ")),
                                                          AccessByPoint
                                                          (Cast
                                                           (TRef ("E"),
                                                            Identifier ("h")),
                                                           CallMethod
                                                           (Identifier ("F"),
                                                            []))));
                                                  Print (Add
                                                         (Value (VString ("h to I .F() = ")),
                                                          AccessByPoint
                                                          (Cast
                                                           (TRef ("IInterface"),
                                                            Identifier ("h")),
                                                           CallMethod
                                                           (Identifier ("F"),
                                                            []))))]));
                           is_overriden = false },
                 "ToString": { method_type = TString;
                               method_modifiers = [Virtual; Public];
                               arguments = []; key = "ToString";
                               interface_key = None;
                               body =
                               Some (StatementBlock ([Return (Some (Value (
                                                                     VString ("Object"))))]));
                               is_overriden = false },
                 ];
                constructors_table =
                ["Program": { key = "Program"; arguments = [];
                              call_constructor = None;
                              body = StatementBlock ([]) },
                 ];
                children_keys = []; is_abstract = false; is_static = false;
                is_class = true; parent_key = ["Object"];
                decl_tree =
                Class
                ([Public], Name ("Program"), [],
                 [([Public; Static],
                   Method
                   (TVoid, Name ("Main"), [],
                    Some (StatementBlock ([VariableDecl
                                           (None, TRef ("A"),
                                            [(Name ("a"),
                                              Some (ClassCreation
                                                    (Name ("A"), [])))]);
                                           Print (Value (VString ("######################################")));
                                           Print (Add
                                                  (Value (VString ("a to A .F() = ")),
                                                   AccessByPoint
                                                   (Cast
                                                    (TRef ("A"),
                                                     Identifier ("a")),
                                                    CallMethod
                                                    (Identifier ("F"), []))));
                                           VariableDecl
                                           (None, TRef ("B"),
                                            [(Name ("b"),
                                              Some (ClassCreation
                                                    (Name ("B"), [])))]);
                                           Print (Value (VString ("######################################")));
                                           Print (Add
                                                  (Value (VString ("b.F() = ")),
                                                   AccessByPoint
                                                   (Identifier ("b"),
                                                    CallMethod
                                                    (Identifier ("F"), []))));
                                           Print (Add
                                                  (Value (VString ("b to A .F() = ")),
                                                   AccessByPoint
                                                   (Cast
                                                    (TRef ("A"),
                                                     Identifier ("b")),
                                                    CallMethod
                                                    (Identifier ("F"), []))));
                                           Print (Add
                                                  (Value (VString ("b to I .F() = ")),
                                                   AccessByPoint
                                                   (Cast
                                                    (TRef ("IInterface"),
                                                     Identifier ("b")),
                                                    CallMethod
                                                    (Identifier ("F"), []))));
                                           VariableDecl
                                           (None, TRef ("C"),
                                            [(Name ("c"),
                                              Some (ClassCreation
                                                    (Name ("C"), [])))]);
                                           Print (Value (VString ("######################################")));
                                           Print (Add
                                                  (Value (VString ("c.F() = ")),
                                                   AccessByPoint
                                                   (Identifier ("c"),
                                                    CallMethod
                                                    (Identifier ("F"), []))));
                                           Print (Add
                                                  (Value (VString ("c to A .F() = ")),
                                                   AccessByPoint
                                                   (Cast
                                                    (TRef ("A"),
                                                     Identifier ("c")),
                                                    CallMethod
                                                    (Identifier ("F"), []))));
                                           Print (Add
                                                  (Value (VString ("c to A .F() = ")),
                                                   AccessByPoint
                                                   (Cast
                                                    (TRef ("B"),
                                                     Identifier ("c")),
                                                    CallMethod
                                                    (Identifier ("F"), []))));
                                           Print (Add
                                                  (Value (VString ("c to I .F() = ")),
                                                   AccessByPoint
                                                   (Cast
                                                    (TRef ("IInterface"),
                                                     Identifier ("c")),
                                                    CallMethod
                                                    (Identifier ("F"), []))));
                                           VariableDecl
                                           (None, TRef ("D"),
                                            [(Name ("d"),
                                              Some (ClassCreation
                                                    (Name ("D"), [])))]);
                                           Print (Value (VString ("######################################")));
                                           Print (Add
                                                  (Value (VString ("d.F() = ")),
                                                   AccessByPoint
                                                   (Identifier ("d"),
                                                    CallMethod
                                                    (Identifier ("F"), []))));
                                           Print (Add
                                                  (Value (VString ("d to A .F() = ")),
                                                   AccessByPoint
                                                   (Cast
                                                    (TRef ("A"),
                                                     Identifier ("d")),
                                                    CallMethod
                                                    (Identifier ("F"), []))));
                                           Print (Add
                                                  (Value (VString ("d to B .F() = ")),
                                                   AccessByPoint
                                                   (Cast
                                                    (TRef ("B"),
                                                     Identifier ("d")),
                                                    CallMethod
                                                    (Identifier ("F"), []))));
                                           Print (Add
                                                  (Value (VString ("d to C .F() = ")),
                                                   AccessByPoint
                                                   (Cast
                                                    (TRef ("C"),
                                                     Identifier ("d")),
                                                    CallMethod
                                                    (Identifier ("F"), []))));
                                           Print (Add
                                                  (Value (VString ("d to I .F() = ")),
                                                   AccessByPoint
                                                   (Cast
                                                    (TRef ("IInterface"),
                                                     Identifier ("d")),
                                                    CallMethod
                                                    (Identifier ("F"), []))));
                                           VariableDecl
                                           (None, TRef ("D1"),
                                            [(Name ("d1"),
                                              Some (ClassCreation
                                                    (Name ("D1"), [])))]);
                                           Print (Value (VString ("######################################")));
                                           Print (Add
                                                  (Value (VString ("d1.F() = ")),
                                                   AccessByPoint
                                                   (Identifier ("d1"),
                                                    CallMethod
                                                    (Identifier ("F"), []))));
                                           Print (Add
                                                  (Value (VString ("d1 to A .F() = ")),
                                                   AccessByPoint
                                                   (Cast
                                                    (TRef ("A"),
                                                     Identifier ("d1")),
                                                    CallMethod
                                                    (Identifier ("F"), []))));
                                           Print (Add
                                                  (Value (VString ("d1 to B .F() = ")),
                                                   AccessByPoint
                                                   (Cast
                                                    (TRef ("B"),
                                                     Identifier ("d1")),
                                                    CallMethod
                                                    (Identifier ("F"), []))));
                                           Print (Add
                                                  (Value (VString ("d1 to C .F() = ")),
                                                   AccessByPoint
                                                   (Cast
                                                    (TRef ("C"),
                                                     Identifier ("d1")),
                                                    CallMethod
                                                    (Identifier ("F"), []))));
                                           Print (Add
                                                  (Value (VString ("d1 to I .F() = ")),
                                                   AccessByPoint
                                                   (Cast
                                                    (TRef ("IInterface"),
                                                     Identifier ("d1")),
                                                    CallMethod
                                                    (Identifier ("F"), []))));
                                           VariableDecl
                                           (None, TRef ("E"),
                                            [(Name ("e"),
                                              Some (ClassCreation
                                                    (Name ("E"), [])))]);
                                           Print (Value (VString ("######################################")));
                                           Print (Add
                                                  (Value (VString ("e.F() = ")),
                                                   AccessByPoint
                                                   (Identifier ("e"),
                                                    CallMethod
                                                    (Identifier ("F"), []))));
                                           Print (Add
                                                  (Value (VString ("e to A .F() = ")),
                                                   AccessByPoint
                                                   (Cast
                                                    (TRef ("A"),
                                                     Identifier ("e")),
                                                    CallMethod
                                                    (Identifier ("F"), []))));
                                           Print (Add
                                                  (Value (VString ("e to B .F() = ")),
                                                   AccessByPoint
                                                   (Cast
                                                    (TRef ("B"),
                                                     Identifier ("e")),
                                                    CallMethod
                                                    (Identifier ("F"), []))));
                                           Print (Add
                                                  (Value (VString ("e to C .F() = ")),
                                                   AccessByPoint
                                                   (Cast
                                                    (TRef ("C"),
                                                     Identifier ("e")),
                                                    CallMethod
                                                    (Identifier ("F"), []))));
                                           Print (Add
                                                  (Value (VString ("e to I .F() = ")),
                                                   AccessByPoint
                                                   (Cast
                                                    (TRef ("IInterface"),
                                                     Identifier ("e")),
                                                    CallMethod
                                                    (Identifier ("F"), []))));
                                           Print (Add
                                                  (Value (VString ("e to A to I .F() = ")),
                                                   AccessByPoint
                                                   (Cast
                                                    (TRef ("IInterface"),
                                                     Cast
                                                     (TRef ("A"),
                                                      Identifier ("e"))),
                                                    CallMethod
                                                    (Identifier ("F"), []))));
                                           Print (Add
                                                  (Value (VString ("e to B to I .F() = ")),
                                                   AccessByPoint
                                                   (Cast
                                                    (TRef ("IInterface"),
                                                     Cast
                                                     (TRef ("B"),
                                                      Identifier ("e"))),
                                                    CallMethod
                                                    (Identifier ("F"), []))));
                                           Print (Add
                                                  (Value (VString ("e to C to I .F() = ")),
                                                   AccessByPoint
                                                   (Cast
                                                    (TRef ("IInterface"),
                                                     Cast
                                                     (TRef ("C"),
                                                      Identifier ("e"))),
                                                    CallMethod
                                                    (Identifier ("F"), []))));
                                           Print (Add
                                                  (Value (VString ("e to B to A .F() = ")),
                                                   AccessByPoint
                                                   (Cast
                                                    (TRef ("A"),
                                                     Cast
                                                     (TRef ("B"),
                                                      Identifier ("e"))),
                                                    CallMethod
                                                    (Identifier ("F"), []))));
                                           Print (Add
                                                  (Value (VString ("e to C to B .F() = ")),
                                                   AccessByPoint
                                                   (Cast
                                                    (TRef ("B"),
                                                     Cast
                                                     (TRef ("C"),
                                                      Identifier ("e"))),
                                                    CallMethod
                                                    (Identifier ("F"), []))));
                                           Print (Add
                                                  (Value (VString ("e to C to A .F() = ")),
                                                   AccessByPoint
                                                   (Cast
                                                    (TRef ("A"),
                                                     Cast
                                                     (TRef ("C"),
                                                      Identifier ("e"))),
                                                    CallMethod
                                                    (Identifier ("F"), []))));
                                           VariableDecl
                                           (None, TRef ("G"),
                                            [(Name ("g"),
                                              Some (ClassCreation
                                                    (Name ("G"), [])))]);
                                           Print (Value (VString ("######################################")));
                                           Print (Add
                                                  (Value (VString ("g.F() = ")),
                                                   AccessByPoint
                                                   (Identifier ("g"),
                                                    CallMethod
                                                    (Identifier ("F"), []))));
                                           Print (Add
                                                  (Value (VString ("g to A .F() = ")),
                                                   AccessByPoint
                                                   (Cast
                                                    (TRef ("A"),
                                                     Identifier ("g")),
                                                    CallMethod
                                                    (Identifier ("F"), []))));
                                           Print (Add
                                                  (Value (VString ("g to B .F() = ")),
                                                   AccessByPoint
                                                   (Cast
                                                    (TRef ("B"),
                                                     Identifier ("g")),
                                                    CallMethod
                                                    (Identifier ("F"), []))));
                                           Print (Add
                                                  (Value (VString ("g to C .F() = ")),
                                                   AccessByPoint
                                                   (Cast
                                                    (TRef ("C"),
                                                     Identifier ("g")),
                                                    CallMethod
                                                    (Identifier ("F"), []))));
                                           Print (Add
                                                  (Value (VString ("g to E .F() = ")),
                                                   AccessByPoint
                                                   (Cast
                                                    (TRef ("E"),
                                                     Identifier ("g")),
                                                    CallMethod
                                                    (Identifier ("F"), []))));
                                           Print (Add
                                                  (Value (VString ("g to I .F() = ")),
                                                   AccessByPoint
                                                   (Cast
                                                    (TRef ("IInterface"),
                                                     Identifier ("g")),
                                                    CallMethod
                                                    (Identifier ("F"), []))));
                                           VariableDecl
                                           (None, TRef ("H"),
                                            [(Name ("h"),
                                              Some (ClassCreation
                                                    (Name ("H"), [])))]);
                                           Print (Value (VString ("######################################")));
                                           Print (Add
                                                  (Value (VString ("h.F() = ")),
                                                   AccessByPoint
                                                   (Identifier ("h"),
                                                    CallMethod
                                                    (Identifier ("F"), []))));
                                           Print (Add
                                                  (Value (VString ("h to A .F() = ")),
                                                   AccessByPoint
                                                   (Cast
                                                    (TRef ("A"),
                                                     Identifier ("h")),
                                                    CallMethod
                                                    (Identifier ("F"), []))));
                                           Print (Add
                                                  (Value (VString ("h to B .F() = ")),
                                                   AccessByPoint
                                                   (Cast
                                                    (TRef ("B"),
                                                     Identifier ("h")),
                                                    CallMethod
                                                    (Identifier ("F"), []))));
                                           Print (Add
                                                  (Value (VString ("h to C .F() = ")),
                                                   AccessByPoint
                                                   (Cast
                                                    (TRef ("C"),
                                                     Identifier ("h")),
                                                    CallMethod
                                                    (Identifier ("F"), []))));
                                           Print (Add
                                                  (Value (VString ("h to E .F() = ")),
                                                   AccessByPoint
                                                   (Cast
                                                    (TRef ("E"),
                                                     Identifier ("h")),
                                                    CallMethod
                                                    (Identifier ("F"), []))));
                                           Print (Add
                                                  (Value (VString ("h to I .F() = ")),
                                                   AccessByPoint
                                                   (Cast
                                                    (TRef ("IInterface"),
                                                     Identifier ("h")),
                                                    CallMethod
                                                    (Identifier ("F"), []))))]))))])
                },
   ]
