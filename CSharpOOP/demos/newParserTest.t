  $ (cd ../../../../default && demos/newParserTest.exe)
  Class
  ([Public], Name ("Program"), [],
   [([Public; Static],
     Method
     (TVoid, Name ("Main"), [],
      Some (StatementBlock ([VariableDecl
                             (None, TRef ("A"),
                              [(Name ("a"),
                                Some (ClassCreation (Name ("A"), [])))]);
                             Print (Value (VString ("######################################")));
                             Print (AccessByPoint
                                    (Cast (TRef ("A"), Identifier ("a")),
                                     CallMethod (Identifier ("F"), [])));
                             VariableDecl
                             (None, TRef ("B"),
                              [(Name ("b"),
                                Some (ClassCreation (Name ("B"), [])))]);
                             Print (Value (VString ("######################################")));
                             Print (AccessByPoint
                                    (Cast (TRef ("A"), Identifier ("b")),
                                     CallMethod (Identifier ("F"), [])));
                             Print (AccessByPoint
                                    (Cast
                                     (TRef ("IInterface"), Identifier ("b")),
                                     CallMethod (Identifier ("F"), [])));
                             VariableDecl
                             (None, TRef ("C"),
                              [(Name ("c"),
                                Some (ClassCreation (Name ("C"), [])))]);
                             Print (Value (VString ("######################################")));
                             Print (AccessByPoint
                                    (Cast (TRef ("A"), Identifier ("c")),
                                     CallMethod (Identifier ("F"), [])));
                             Print (AccessByPoint
                                    (Cast
                                     (TRef ("IInterface"), Identifier ("c")),
                                     CallMethod (Identifier ("F"), [])));
                             VariableDecl
                             (None, TRef ("D"),
                              [(Name ("d"),
                                Some (ClassCreation (Name ("D"), [])))]);
                             Print (Value (VString ("######################################")));
                             Print (AccessByPoint
                                    (Cast (TRef ("A"), Identifier ("d")),
                                     CallMethod (Identifier ("F"), [])));
                             Print (AccessByPoint
                                    (Identifier ("d"),
                                     CallMethod (Identifier ("F"), [])));
                             Print (AccessByPoint
                                    (Cast
                                     (TRef ("IInterface"), Identifier ("d")),
                                     CallMethod (Identifier ("F"), [])));
                             VariableDecl
                             (None, TRef ("D1"),
                              [(Name ("d1"),
                                Some (ClassCreation (Name ("D1"), [])))]);
                             Print (Value (VString ("######################################")));
                             Print (AccessByPoint
                                    (Cast
                                     (TRef ("IInterface"), Identifier ("d1")),
                                     CallMethod (Identifier ("F"), [])));
                             VariableDecl
                             (None, TRef ("E"),
                              [(Name ("e"),
                                Some (ClassCreation (Name ("E"), [])))]);
                             Print (Value (VString ("######################################")));
                             Print (AccessByPoint
                                    (Cast (TRef ("A"), Identifier ("e")),
                                     CallMethod (Identifier ("F"), [])));
                             Print (AccessByPoint
                                    (Identifier ("e"),
                                     CallMethod (Identifier ("F"), [])));
                             Print (AccessByPoint
                                    (Cast
                                     (TRef ("IInterface"), Identifier ("e")),
                                     CallMethod (Identifier ("F"), [])));
                             Print (AccessByPoint
                                    (Cast
                                     (TRef ("IInterface"),
                                      Cast (TRef ("B"), Identifier ("e"))),
                                     CallMethod (Identifier ("F"), [])));
                             VariableDecl
                             (None, TRef ("G"),
                              [(Name ("g"),
                                Some (ClassCreation (Name ("G"), [])))]);
                             Print (Value (VString ("######################################")));
                             Print (AccessByPoint
                                    (Cast (TRef ("A"), Identifier ("g")),
                                     CallMethod (Identifier ("F"), [])));
                             Print (AccessByPoint
                                    (Cast
                                     (TRef ("IInterface"), Identifier ("g")),
                                     CallMethod (Identifier ("F"), [])));
                             Print (AccessByPoint
                                    (Cast (TRef ("E"), Identifier ("g")),
                                     CallMethod (Identifier ("F"), [])))]))))])
  Interface
  (Public, Name ("IInterface"), [],
   [([Public], Method (TInt, Name ("F"), [], None))])
  Class
  ([Public], Name ("A"), [],
   [([Public; Virtual],
     Method
     (TInt, Name ("F"), [],
      Some (StatementBlock ([Return (Some (Value (VInt (7))))]))))])
  Class
  ([Public], Name ("B"), [Name ("A"); Name ("IInterface")],
   [([Public; Override],
     Method
     (TInt, Name ("F"), [],
      Some (StatementBlock ([Return (Some (Value (VInt (8))))]))))])
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
  Class
  ([Public], Name ("D"), [Name ("C")],
   [([Public; New],
     Method
     (TInt, Name ("F"), [],
      Some (StatementBlock ([Return (Some (Sub
                                           (Value (VInt (0)),
                                            Value (VInt (10)))))]))))])
  Class
  ([Public], Name ("D1"), [Name ("C"); Name ("IInterface")],
   [([Public; New],
     Method
     (TInt, Name ("F"), [],
      Some (StatementBlock ([Return (Some (Sub
                                           (Value (VInt (0)),
                                            Value (VInt (11)))))]))))])
  Class
  ([Public], Name ("E"), [Name ("C")],
   [([Public; New; Virtual],
     Method
     (TInt, Name ("F"), [],
      Some (StatementBlock ([Return (Some (Value (VInt (10))))]))))])
  Class
  ([Public], Name ("G"), [Name ("E"); Name ("IInterface")],
   [([Public; Override],
     Method
     (TInt, Name ("F"), [],
      Some (StatementBlock ([Return (Some (Value (VInt (11))))]))))])
