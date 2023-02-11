  $ (cd ../../../../default && demos/classLoaderTest.exe)
  -_-_-_-_-_-_-_-_-_-_- Wrong modifier testing -_-_-_-_-_-_-_-_-_-_-
  
  Public, protected and private is mutually exclusive modifiers for field
  
  -_-_-_-_-_-_-_-_-_-_- Simple object testing -_-_-_-_-_-_-_-_-_-_-
  
  ["A": { this_key = "A";
          fields_table =
          ["a": { field_type = TInt; field_modifiers = [Public]; key = "a";
                  sub_tree = None },
           ];
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
           "MethodTInt": { method_type = TInt; method_modifiers = [Private];
                           arguments = [(TInt, Name ("b"))];
                           key = "MethodTInt"; interface_key = None;
                           body = Some (StatementBlock ([]));
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
          children_keys = []; is_abstract = false; is_static = false;
          is_class = true; parent_key = ["Object"];
          decl_tree =
          Class
          ([Public], Name ("A"), [],
           [([Public], Field (TInt, [(Name ("a"), None)]));
            ([Private],
             Method
             (TInt, Name ("Method"), [(TInt, Name ("b"))],
              Some (StatementBlock ([]))))])
          },
   "B": { this_key = "B";
          fields_table =
          ["a": { field_type = TInt; field_modifiers = [Private]; key = "a";
                  sub_tree = None },
           ];
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
          children_keys = []; is_abstract = false; is_static = false;
          is_class = true; parent_key = ["Object"];
          decl_tree =
          Class
          ([Public], Name ("B"), [],
           [([Private], Field (TInt, [(Name ("a"), None)]))])
          },
   "C": { this_key = "C";
          fields_table =
          ["a": { field_type = TInt; field_modifiers = [Protected]; key = "a";
                  sub_tree = None },
           ];
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
          children_keys = []; is_abstract = false; is_static = false;
          is_class = true; parent_key = ["Object"];
          decl_tree =
          Class
          ([Public], Name ("C"), [],
           [([Protected], Field (TInt, [(Name ("a"), None)]))])
          },
   "D": { this_key = "D"; fields_table = [];
          methods_table =
          ["a": { method_type = TInt;
                  method_modifiers = [Abstract; Virtual; Public];
                  arguments = []; key = "a"; interface_key = None; body = None;
                  is_overriden = false },
           ];
          constructors_table = []; children_keys = []; is_abstract = false;
          is_static = false; is_class = false; parent_key = ["Object"];
          decl_tree =
          Interface
          (Public, Name ("D"), [],
           [([Public], Method (TInt, Name ("a"), [], None))])
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
               children_keys = ["Program"; "D"; "C"; "B"; "A"];
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
                           body = Some (StatementBlock ([]));
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
                   (TVoid, Name ("Main"), [], Some (StatementBlock ([]))))])
                },
   ]
  -_-_-_-_-_-_-_-_-_-_- Interface inheritance error testing -_-_-_-_-_-_-_-_-_-_-
  
  Class A doesn't realize parent I interface F method
  
  -_-_-_-_-_-_-_-_-_-_- Hide method testing -_-_-_-_-_-_-_-_-_-_-
  
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
                                                                Return (
                                                                 Some (
                                                                  Value (
                                                                   VInt (1)))),
                                                                Some (Return (
                                                                      Some (
                                                                      Value (
                                                                      VInt (0))))))]));
                                        is_overriden = false },
             "F": { method_type = TInt; method_modifiers = [Public];
                    arguments = []; key = "F"; interface_key = None;
                    body =
                    Some (StatementBlock ([Return (Some (Value (VInt (1))))]));
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
            ["A": { key = "A"; arguments = []; call_constructor = None;
                    body = StatementBlock ([]) },
             ];
            children_keys = ["B"]; is_abstract = false; is_static = false;
            is_class = true; parent_key = ["Object"];
            decl_tree =
            Class
            ([Public], Name ("A"), [],
             [([Public],
               Method
               (TInt, Name ("F"), [],
                Some (StatementBlock ([Return (Some (Value (VInt (1))))]))))])
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
                                                                Return (
                                                                 Some (
                                                                  Value (
                                                                   VInt (1)))),
                                                                Some (Return (
                                                                      Some (
                                                                      Value (
                                                                      VInt (0))))))]));
                                        is_overriden = false },
             "F": { method_type = TInt; method_modifiers = [Public];
                    arguments = []; key = "F"; interface_key = None;
                    body =
                    Some (StatementBlock ([Return (Some (Value (VInt (2))))]));
                    is_overriden = true },
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
            ["B": { key = "B"; arguments = []; call_constructor = None;
                    body = StatementBlock ([]) },
             ];
            children_keys = []; is_abstract = false; is_static = false;
            is_class = true; parent_key = ["A"];
            decl_tree =
            Class
            ([Public], Name ("B"), [Name ("A")],
             [([Public],
               Method
               (TInt, Name ("F"), [],
                Some (StatementBlock ([Return (Some (Value (VInt (2))))]))))])
            },
     "Object": { this_key = "Object"; fields_table = [];
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
                              call_constructor = None;
                              body = StatementBlock ([]) },
                  ];
                 children_keys = ["Program"; "A"]; is_abstract = false;
                 is_static = false; is_class = true; parent_key = [];
                 decl_tree =
                 Class
                 ([Public], Name ("Object"), [],
                  [([Public],
                    Method
                    (TInt, Name ("Equals"), [(TRef ("Object"), Name ("obj"))],
                     Some (StatementBlock ([If
                                            (Equal (This, Identifier ("obj")),
                                             Return (Some (Value (VInt (1)))),
                                             Some (Return (Some (Value (
                                                                  VInt (0))))))]))));
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
                             method_modifiers = [Public; Static];
                             arguments = []; key = "Main";
                             interface_key = None;
                             body = Some (StatementBlock ([]));
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
                     (TVoid, Name ("Main"), [], Some (StatementBlock ([]))))])
                  },
     ]
  -_-_-_-_-_-_-_-_-_-_- This constructor key testing -_-_-_-_-_-_-_-_-_-_-
  
  ["A": { this_key = "A";
              fields_table =
              ["a": { field_type = TInt; field_modifiers = [Public]; key = "a";
                      sub_tree = None },
               ];
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
               "MethodTInt": { method_type = TInt;
                               method_modifiers = [Private];
                               arguments = [(TInt, Name ("b"))];
                               key = "MethodTInt"; interface_key = None;
                               body = Some (StatementBlock ([]));
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
              ["A": { key = "A"; arguments = []; call_constructor = None;
                      body = StatementBlock ([]) },
               "ATInt": { key = "ATInt"; arguments = [(TInt, Name ("k"))];
                          call_constructor = Some (CallMethod (This, []));
                          body = StatementBlock ([]) },
               ];
              children_keys = ["B"]; is_abstract = false; is_static = false;
              is_class = true; parent_key = ["Object"];
              decl_tree =
              Class
              ([Public], Name ("A"), [],
               [([Public], Field (TInt, [(Name ("a"), None)]));
                ([Private],
                 Method
                 (TInt, Name ("Method"), [(TInt, Name ("b"))],
                  Some (StatementBlock ([]))));
                ([Public],
                 Constructor (Name ("A"), [], None, StatementBlock ([])));
                ([Public],
                 Constructor
                 (Name ("A"), [(TInt, Name ("k"))],
                  Some (CallMethod (This, [])), StatementBlock ([])))])
              },
       "B": { this_key = "B";
              fields_table =
              ["a": { field_type = TInt; field_modifiers = [Public]; key = "a";
                      sub_tree = None },
               ];
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
               "MethodTInt": { method_type = TInt;
                               method_modifiers = [Private];
                               arguments = [(TInt, Name ("b"))];
                               key = "MethodTInt"; interface_key = None;
                               body = Some (StatementBlock ([]));
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
              ["B": { key = "B"; arguments = []; call_constructor = None;
                      body = StatementBlock ([]) },
               ];
              children_keys = []; is_abstract = false; is_static = false;
              is_class = true; parent_key = ["A"];
              decl_tree = Class ([Public], Name ("B"), [Name ("A")], []) },
       "Object": { this_key = "Object"; fields_table = [];
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
                    "ToString": { method_type = TString;
                                  method_modifiers = [Virtual; Public];
                                  arguments = []; key = "ToString";
                                  interface_key = None;
                                  body =
                                  Some (StatementBlock ([Return (Some (
                                                                  Value (
                                                                   VString ("Object"))))]));
                                  is_overriden = false },
                    ];
                   constructors_table =
                   ["Object": { key = "Object"; arguments = [];
                                call_constructor = None;
                                body = StatementBlock ([]) },
                    ];
                   children_keys = ["Program"; "A"]; is_abstract = false;
                   is_static = false; is_class = true; parent_key = [];
                   decl_tree =
                   Class
                   ([Public], Name ("Object"), [],
                    [([Public],
                      Method
                      (TInt, Name ("Equals"),
                       [(TRef ("Object"), Name ("obj"))],
                       Some (StatementBlock ([If
                                              (Equal (This, Identifier ("obj")),
                                               Return (Some (Value (VInt (1)))),
                                               Some (Return (Some (Value (
                                                                    VInt (0))))))]))));
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
                                                [(TRef ("Object"),
                                                  Name ("obj"))];
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
                               method_modifiers = [Public; Static];
                               arguments = []; key = "Main";
                               interface_key = None;
                               body = Some (StatementBlock ([]));
                               is_overriden = false },
                     "ToString": { method_type = TString;
                                   method_modifiers = [Virtual; Public];
                                   arguments = []; key = "ToString";
                                   interface_key = None;
                                   body =
                                   Some (StatementBlock ([Return (Some (
                                                                   Value (
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
                       (TVoid, Name ("Main"), [], Some (StatementBlock ([]))))])
                    },
       ]
  -_-_-_-_-_-_-_-_-_-_- Inheritance testing -_-_-_-_-_-_-_-_-_-_-
  
  ["Child": { this_key = "Child";
                    fields_table =
                    ["age": { field_type = TInt; field_modifiers = [Public];
                              key = "age"; sub_tree = None },
                     "cash": { field_type = TInt; field_modifiers = [Public];
                               key = "cash"; sub_tree = None },
                     "weight": { field_type = TInt; field_modifiers = [Public];
                                 key = "weight"; sub_tree = None },
                     ];
                    methods_table =
                    ["EqualsTRef ("Object")": { method_type = TInt;
                                                method_modifiers =
                                                [Virtual; Public];
                                                arguments =
                                                [(TRef ("Object"),
                                                  Name ("obj"))];
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
                     "GetAge": { method_type = TInt;
                                 method_modifiers = [Public; Override];
                                 arguments = []; key = "GetAge";
                                 interface_key = None;
                                 body =
                                 Some (StatementBlock ([Return (Some (Add
                                                                      (Identifier ("age"),
                                                                      Value (
                                                                      VInt (1)))))]));
                                 is_overriden = true },
                     "GetCash": { method_type = TInt;
                                  method_modifiers = [Public]; arguments = [];
                                  key = "GetCash"; interface_key = None;
                                  body =
                                  Some (StatementBlock ([Return (Some (
                                                                  Identifier ("cash")))]));
                                  is_overriden = false },
                     "GetWeight": { method_type = TInt;
                                    method_modifiers = [Public];
                                    arguments = []; key = "GetWeight";
                                    interface_key = None;
                                    body =
                                    Some (StatementBlock ([Return (Some (
                                                                    Identifier ("weight")))]));
                                    is_overriden = false },
                     "SetAgeTInt": { method_type = TVoid;
                                     method_modifiers = [Public];
                                     arguments = [(TInt, Name ("age"))];
                                     key = "SetAgeTInt"; interface_key = None;
                                     body =
                                     Some (StatementBlock ([Expression (
                                                             Assign
                                                             (AccessByPoint
                                                              (This,
                                                               Identifier ("age")),
                                                              Identifier ("age")))]));
                                     is_overriden = false },
                     "SetCashTInt": { method_type = TVoid;
                                      method_modifiers = [Public];
                                      arguments = [(TInt, Name ("cash"))];
                                      key = "SetCashTInt";
                                      interface_key = None;
                                      body =
                                      Some (StatementBlock ([Expression (
                                                              Assign
                                                              (AccessByPoint
                                                               (This,
                                                                Identifier ("cash")),
                                                               Identifier ("cash")))]));
                                      is_overriden = false },
                     "SetWeightTInt": { method_type = TVoid;
                                        method_modifiers = [Public];
                                        arguments = [(TInt, Name ("weight"))];
                                        key = "SetWeightTInt";
                                        interface_key = None;
                                        body =
                                        Some (StatementBlock ([Expression (
                                                                Assign
                                                                (AccessByPoint
                                                                 (This,
                                                                  Identifier ("weight")),
                                                                 Identifier ("weight")))]));
                                        is_overriden = false },
                     "TellEvenNumbersTInt": { method_type = TVoid;
                                              method_modifiers = [Public];
                                              arguments =
                                              [(TInt, Name ("count"))];
                                              key = "TellEvenNumbersTInt";
                                              interface_key = None;
                                              body =
                                              Some (StatementBlock ([For
                                                                     (Some (
                                                                      VariableDecl
                                                                      (None,
                                                                      TInt,
                                                                      [(Name ("i"),
                                                                      Some (
                                                                      Value (
                                                                      VInt (0))))])),
                                                                      Some (
                                                                      Less
                                                                      (Identifier ("i"),
                                                                      Identifier ("count"))),
                                                                      [PostInc (
                                                                      Identifier ("i"))],
                                                                      StatementBlock (
                                                                      [If
                                                                      (And
                                                                      (Equal
                                                                      (Mod
                                                                      (Identifier ("i"),
                                                                      Value (
                                                                      VInt (2))),
                                                                      Value (
                                                                      VInt (0))),
                                                                      Not (
                                                                      Equal
                                                                      (Mod
                                                                      (Identifier ("i"),
                                                                      Value (
                                                                      VInt (2))),
                                                                      Value (
                                                                      VInt (1))))),
                                                                      StatementBlock (
                                                                      [Print (
                                                                      Identifier ("i"))]),
                                                                      Some (
                                                                      StatementBlock (
                                                                      [Continue])))]))]));
                                              is_overriden = false },
                     "ToString": { method_type = TString;
                                   method_modifiers = [Virtual; Public];
                                   arguments = []; key = "ToString";
                                   interface_key = None;
                                   body =
                                   Some (StatementBlock ([Return (Some (
                                                                   Value (
                                                                    VString ("Object"))))]));
                                   is_overriden = false },
                     ];
                    constructors_table =
                    ["ChildTIntTInt": { key = "ChildTIntTInt";
                                        arguments =
                                        [(TInt, Name ("weight"));
                                         (TInt, Name ("age"))];
                                        call_constructor =
                                        Some (CallMethod
                                              (Base,
                                               [Identifier ("weight");
                                                Identifier ("age")]));
                                        body =
                                        StatementBlock ([Expression (Assign
                                                                     (Identifier ("cash"),
                                                                      Value (
                                                                      VInt (0))))])
                                        },
                     "ChildTIntTIntTInt": { key = "ChildTIntTIntTInt";
                                            arguments =
                                            [(TInt, Name ("weight"));
                                             (TInt, Name ("age"));
                                             (TInt, Name ("cash"))];
                                            call_constructor =
                                            Some (CallMethod
                                                  (Base,
                                                   [Identifier ("weight");
                                                    Identifier ("age")]));
                                            body =
                                            StatementBlock ([Expression (
                                                              Assign
                                                              (AccessByPoint
                                                               (This,
                                                                Identifier ("cash")),
                                                               Identifier ("cash")))])
                                            },
                     ];
                    children_keys = []; is_abstract = false; is_static = false;
                    is_class = true; parent_key = ["Person"];
                    decl_tree =
                    Class
                    ([Public], Name ("Child"), [Name ("Person")],
                     [([Public], Field (TInt, [(Name ("cash"), None)]));
                      ([Public],
                       Constructor
                       (Name ("Child"),
                        [(TInt, Name ("weight")); (TInt, Name ("age"))],
                        Some (CallMethod
                              (Base,
                               [Identifier ("weight"); Identifier ("age")])),
                        StatementBlock ([Expression (Assign
                                                     (Identifier ("cash"),
                                                      Value (VInt (0))))])));
                      ([Public],
                       Constructor
                       (Name ("Child"),
                        [(TInt, Name ("weight")); (TInt, Name ("age"));
                         (TInt, Name ("cash"))],
                        Some (CallMethod
                              (Base,
                               [Identifier ("weight"); Identifier ("age")])),
                        StatementBlock ([Expression (Assign
                                                     (AccessByPoint
                                                      (This,
                                                       Identifier ("cash")),
                                                      Identifier ("cash")))])));
                      ([Public; Override],
                       Method
                       (TInt, Name ("GetAge"), [],
                        Some (StatementBlock ([Return (Some (Add
                                                             (Identifier ("age"),
                                                              Value (VInt (1)))))]))));
                      ([Public],
                       Method
                       (TInt, Name ("GetCash"), [],
                        Some (StatementBlock ([Return (Some (Identifier ("cash")))]))));
                      ([Public],
                       Method
                       (TVoid, Name ("SetCash"), [(TInt, Name ("cash"))],
                        Some (StatementBlock ([Expression (Assign
                                                           (AccessByPoint
                                                            (This,
                                                             Identifier ("cash")),
                                                            Identifier ("cash")))]))));
                      ([Public],
                       Method
                       (TVoid, Name ("TellEvenNumbers"),
                        [(TInt, Name ("count"))],
                        Some (StatementBlock ([For
                                               (Some (VariableDecl
                                                      (None, TInt,
                                                       [(Name ("i"),
                                                         Some (Value (VInt (0))))])),
                                                Some (Less
                                                      (Identifier ("i"),
                                                       Identifier ("count"))),
                                                [PostInc (Identifier ("i"))],
                                                StatementBlock ([If
                                                                 (And
                                                                  (Equal
                                                                   (Mod
                                                                    (Identifier ("i"),
                                                                     Value (
                                                                      VInt (2))),
                                                                    Value (
                                                                     VInt (0))),
                                                                   Not (
                                                                    Equal
                                                                    (Mod
                                                                     (Identifier ("i"),
                                                                      Value (
                                                                      VInt (2))),
                                                                     Value (
                                                                      VInt (1))))),
                                                                  StatementBlock (
                                                                   [Print (
                                                                     Identifier ("i"))]),
                                                                  Some (
                                                                   StatementBlock (
                                                                    [Continue])))]))]))))])
                    },
         "Object": { this_key = "Object"; fields_table = [];
                     methods_table =
                     ["EqualsTRef ("Object")": { method_type = TInt;
                                                 method_modifiers =
                                                 [Virtual; Public];
                                                 arguments =
                                                 [(TRef ("Object"),
                                                   Name ("obj"))];
                                                 key =
                                                 "EqualsTRef (\"Object\")";
                                                 interface_key = None;
                                                 body =
                                                 Some (StatementBlock (
                                                        [If
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
                      "ToString": { method_type = TString;
                                    method_modifiers = [Virtual; Public];
                                    arguments = []; key = "ToString";
                                    interface_key = None;
                                    body =
                                    Some (StatementBlock ([Return (Some (
                                                                    Value (
                                                                     VString ("Object"))))]));
                                    is_overriden = false },
                      ];
                     constructors_table =
                     ["Object": { key = "Object"; arguments = [];
                                  call_constructor = None;
                                  body = StatementBlock ([]) },
                      ];
                     children_keys = ["Program"; "Person"];
                     is_abstract = false; is_static = false; is_class = true;
                     parent_key = [];
                     decl_tree =
                     Class
                     ([Public], Name ("Object"), [],
                      [([Public],
                        Method
                        (TInt, Name ("Equals"),
                         [(TRef ("Object"), Name ("obj"))],
                         Some (StatementBlock ([If
                                                (Equal
                                                 (This, Identifier ("obj")),
                                                 Return (Some (Value (VInt (1)))),
                                                 Some (Return (Some (Value (
                                                                      VInt (0))))))]))));
                       ([Public],
                        Method
                        (TString, Name ("ToString"), [],
                         Some (StatementBlock ([Return (Some (Value (VString ("Object"))))]))))])
                     },
         "Person": { this_key = "Person";
                     fields_table =
                     ["age": { field_type = TInt; field_modifiers = [Public];
                               key = "age"; sub_tree = None },
                      "weight": { field_type = TInt;
                                  field_modifiers = [Public]; key = "weight";
                                  sub_tree = None },
                      ];
                     methods_table =
                     ["EqualsTRef ("Object")": { method_type = TInt;
                                                 method_modifiers =
                                                 [Virtual; Public];
                                                 arguments =
                                                 [(TRef ("Object"),
                                                   Name ("obj"))];
                                                 key =
                                                 "EqualsTRef (\"Object\")";
                                                 interface_key = None;
                                                 body =
                                                 Some (StatementBlock (
                                                        [If
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
                      "GetAge": { method_type = TInt;
                                  method_modifiers = [Public; Virtual];
                                  arguments = []; key = "GetAge";
                                  interface_key = None;
                                  body =
                                  Some (StatementBlock ([Return (Some (
                                                                  Identifier ("age")))]));
                                  is_overriden = false },
                      "GetWeight": { method_type = TInt;
                                     method_modifiers = [Public];
                                     arguments = []; key = "GetWeight";
                                     interface_key = None;
                                     body =
                                     Some (StatementBlock ([Return (Some (
                                                                     Identifier ("weight")))]));
                                     is_overriden = false },
                      "SetAgeTInt": { method_type = TVoid;
                                      method_modifiers = [Public];
                                      arguments = [(TInt, Name ("age"))];
                                      key = "SetAgeTInt"; interface_key = None;
                                      body =
                                      Some (StatementBlock ([Expression (
                                                              Assign
                                                              (AccessByPoint
                                                               (This,
                                                                Identifier ("age")),
                                                               Identifier ("age")))]));
                                      is_overriden = false },
                      "SetWeightTInt": { method_type = TVoid;
                                         method_modifiers = [Public];
                                         arguments = [(TInt, Name ("weight"))];
                                         key = "SetWeightTInt";
                                         interface_key = None;
                                         body =
                                         Some (StatementBlock ([Expression (
                                                                 Assign
                                                                 (AccessByPoint
                                                                  (This,
                                                                   Identifier ("weight")),
                                                                  Identifier ("weight")))]));
                                         is_overriden = false },
                      "ToString": { method_type = TString;
                                    method_modifiers = [Virtual; Public];
                                    arguments = []; key = "ToString";
                                    interface_key = None;
                                    body =
                                    Some (StatementBlock ([Return (Some (
                                                                    Value (
                                                                     VString ("Object"))))]));
                                    is_overriden = false },
                      ];
                     constructors_table =
                     ["PersonTIntTInt": { key = "PersonTIntTInt";
                                          arguments =
                                          [(TInt, Name ("weight"));
                                           (TInt, Name ("age"))];
                                          call_constructor = None;
                                          body =
                                          StatementBlock ([Expression (
                                                            Assign
                                                            (AccessByPoint
                                                             (This,
                                                              Identifier ("weight")),
                                                             Identifier ("weight")));
                                                           Expression (
                                                            Assign
                                                            (AccessByPoint
                                                             (This,
                                                              Identifier ("age")),
                                                             Identifier ("age")))])
                                          },
                      ];
                     children_keys = ["Child"]; is_abstract = false;
                     is_static = false; is_class = true;
                     parent_key = ["Object"];
                     decl_tree =
                     Class
                     ([Public], Name ("Person"), [],
                      [([Public], Field (TInt, [(Name ("weight"), None)]));
                       ([Public], Field (TInt, [(Name ("age"), None)]));
                       ([Public],
                        Constructor
                        (Name ("Person"),
                         [(TInt, Name ("weight")); (TInt, Name ("age"))], 
                         None,
                         StatementBlock ([Expression (Assign
                                                      (AccessByPoint
                                                       (This,
                                                        Identifier ("weight")),
                                                       Identifier ("weight")));
                                          Expression (Assign
                                                      (AccessByPoint
                                                       (This,
                                                        Identifier ("age")),
                                                       Identifier ("age")))])));
                       ([Public],
                        Method
                        (TInt, Name ("GetWeight"), [],
                         Some (StatementBlock ([Return (Some (Identifier ("weight")))]))));
                       ([Public],
                        Method
                        (TVoid, Name ("SetWeight"), [(TInt, Name ("weight"))],
                         Some (StatementBlock ([Expression (Assign
                                                            (AccessByPoint
                                                             (This,
                                                              Identifier ("weight")),
                                                             Identifier ("weight")))]))));
                       ([Public; Virtual],
                        Method
                        (TInt, Name ("GetAge"), [],
                         Some (StatementBlock ([Return (Some (Identifier ("age")))]))));
                       ([Public],
                        Method
                        (TVoid, Name ("SetAge"), [(TInt, Name ("age"))],
                         Some (StatementBlock ([Expression (Assign
                                                            (AccessByPoint
                                                             (This,
                                                              Identifier ("age")),
                                                             Identifier ("age")))]))))])
                     },
         "Program": { this_key = "Program"; fields_table = [];
                      methods_table =
                      ["EqualsTRef ("Object")": { method_type = TInt;
                                                  method_modifiers =
                                                  [Virtual; Public];
                                                  arguments =
                                                  [(TRef ("Object"),
                                                    Name ("obj"))];
                                                  key =
                                                  "EqualsTRef (\"Object\")";
                                                  interface_key = None;
                                                  body =
                                                  Some (StatementBlock (
                                                         [If
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
                       "Main": { method_type = TVoid;
                                 method_modifiers = [Public; Static];
                                 arguments = []; key = "Main";
                                 interface_key = None;
                                 body =
                                 Some (StatementBlock ([VariableDecl
                                                        (None, TRef ("Person"),
                                                         [(Name ("person"),
                                                           Some (ClassCreation
                                                                 (Name ("Person"),
                                                                  [Value (
                                                                    VInt (100));
                                                                   Value (
                                                                    VInt (50))])))])]));
                                 is_overriden = false },
                       "ToString": { method_type = TString;
                                     method_modifiers = [Virtual; Public];
                                     arguments = []; key = "ToString";
                                     interface_key = None;
                                     body =
                                     Some (StatementBlock ([Return (Some (
                                                                     Value (
                                                                      VString ("Object"))))]));
                                     is_overriden = false },
                       ];
                      constructors_table =
                      ["Program": { key = "Program"; arguments = [];
                                    call_constructor = None;
                                    body = StatementBlock ([]) },
                       ];
                      children_keys = []; is_abstract = false;
                      is_static = false; is_class = true;
                      parent_key = ["Object"];
                      decl_tree =
                      Class
                      ([Public], Name ("Program"), [],
                       [([Public; Static],
                         Method
                         (TVoid, Name ("Main"), [],
                          Some (StatementBlock ([VariableDecl
                                                 (None, TRef ("Person"),
                                                  [(Name ("person"),
                                                    Some (ClassCreation
                                                          (Name ("Person"),
                                                           [Value (VInt (100));
                                                            Value (VInt (50))])))])]))))])
                      },
         ]
  -_-_-_-_-_-_-_-_-_-_- Wrong modifiers testing -_-_-_-_-_-_-_-_-_-_-
  
  Methods cannot be const
  Wrong class modifiers
  Wrong constructor modifiers
  
  -_-_-_-_-_-_-_-_-_-_- Similar fields error testing -_-_-_-_-_-_-_-_-_-_-
  
  The field with this key: age already exists
  
  -_-_-_-_-_-_-_-_-_-_- Similar methods error testing -_-_-_-_-_-_-_-_-_-_-
  
  The method with this key: SetWeightTInt already exists
  
  -_-_-_-_-_-_-_-_-_-_- Similar constructors error testing -_-_-_-_-_-_-_-_-_-_-
  
  The constructor with this key: PersonTIntTInt already exists
  
  -_-_-_-_-_-_-_-_-_-_- Abstract errors testing -_-_-_-_-_-_-_-_-_-_-
  
  Abstract method in non-abstract class
  Body missing in non-abstract method
  Abstract method cannot have body
  Abstract method buildTRef ("Builder") must be overriden
  
  -_-_-_-_-_-_-_-_-_-_- Override errors testing -_-_-_-_-_-_-_-_-_-_-
  
  Cannot override non-existent GetCash method in parent
  Cannot override non-virtual or non-abstract SetAgeTInt method in Person parent class
