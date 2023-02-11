  $ (cd ../../../../default && demos/parserTest.exe)
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
                                       [Value (VInt (100)); Value (VInt (50))])))]);
                             Expression (AccessByPoint
                                         (Identifier ("person"),
                                          CallMethod
                                          (Identifier ("SetAge"),
                                           [Value (VInt (45))])));
                             Print (AccessByPoint
                                    (Identifier ("person"),
                                     CallMethod (Identifier ("GetAge"), [])));
                             VariableDecl
                             (None, TRef ("Child"),
                              [(Name ("child"),
                                Some (ClassCreation
                                      (Name ("Child"),
                                       [Value (VInt (50)); Value (VInt (10))])))]);
                             Expression (AccessByPoint
                                         (Identifier ("child"),
                                          CallMethod
                                          (Identifier ("SetCash"),
                                           [Value (VInt (1000))])));
                             Print (AccessByPoint
                                    (Identifier ("child"),
                                     CallMethod (Identifier ("GetCash"), [])));
                             Expression (AccessByPoint
                                         (Identifier ("child"),
                                          CallMethod
                                          (Identifier ("TellEvenNumbers"),
                                           [Value (VInt (333))])))]))))])
  Class
  ([Public], Name ("Person"), [],
   [([Public], Field (TInt, [(Name ("weight"), None)]));
    ([Public], Field (TInt, [(Name ("age"), None)]));
    ([Public],
     Constructor
     (Name ("Person"), [(TInt, Name ("weight")); (TInt, Name ("age"))], 
      None,
      StatementBlock ([Expression (Assign
                                   (AccessByPoint (This, Identifier ("weight")),
                                    Identifier ("weight")));
                       Expression (Assign
                                   (AccessByPoint (This, Identifier ("age")),
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
                                          (This, Identifier ("weight")),
                                          Identifier ("weight")))]))));
    ([Public],
     Method
     (TInt, Name ("GetAge"), [],
      Some (StatementBlock ([Return (Some (Identifier ("age")))]))));
    ([Public],
     Method
     (TVoid, Name ("SetAge"), [(TInt, Name ("age"))],
      Some (StatementBlock ([Expression (Assign
                                         (AccessByPoint
                                          (This, Identifier ("age")),
                                          Identifier ("age")))]))))])
  Class
  ([Public], Name ("Child"), [Name ("Person")],
   [([Public], Field (TInt, [(Name ("cash"), None)]));
    ([Public],
     Constructor
     (Name ("Child"), [(TInt, Name ("weight")); (TInt, Name ("age"))],
      Some (CallMethod (Base, [Identifier ("weight"); Identifier ("age")])),
      StatementBlock ([Expression (Assign
                                   (Identifier ("cash"), Value (VInt (0))))])));
    ([Public],
     Constructor
     (Name ("Child"),
      [(TInt, Name ("weight")); (TInt, Name ("age")); (TInt, Name ("cash"))],
      Some (CallMethod (This, [Identifier ("weight"); Identifier ("age")])),
      StatementBlock ([Expression (Assign
                                   (AccessByPoint (This, Identifier ("cash")),
                                    Identifier ("cash")))])));
    ([Public],
     Method
     (TInt, Name ("GetCash"), [],
      Some (StatementBlock ([Return (Some (Identifier ("cash")))]))));
    ([Public],
     Method
     (TVoid, Name ("SetCash"), [(TInt, Name ("cash"))],
      Some (StatementBlock ([Expression (Assign
                                         (AccessByPoint
                                          (This, Identifier ("cash")),
                                          Identifier ("cash")))]))));
    ([Public],
     Method
     (TVoid, Name ("TellEvenNumbers"), [(TInt, Name ("count"))],
      Some (StatementBlock ([For
                             (Some (VariableDecl
                                    (None, TInt,
                                     [(Name ("i"), Some (Value (VInt (0))))])),
                              Some (Less
                                    (Identifier ("i"), Identifier ("count"))),
                              [PostInc (Identifier ("i"))],
                              StatementBlock ([If
                                               (And
                                                (Equal
                                                 (Mod
                                                  (Identifier ("i"),
                                                   Value (VInt (2))),
                                                  Value (VInt (0))),
                                                 Not (Equal
                                                      (Mod
                                                       (Identifier ("i"),
                                                        Value (VInt (2))),
                                                       Value (VInt (1))))),
                                                StatementBlock ([Print (
                                                                  Identifier ("i"))]),
                                                Some (StatementBlock ([Continue])))]))]))))])
