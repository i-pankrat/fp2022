
Tests about parsing go here. It's expected that programs parse something and
output a parse tree.
For example, where your test correctness of AST it's recommend to put both
input and output into this file. In this case it will be easier to check that
answer is correct

  $ ./demoParse.exe <<-EOF
  > int main(int number, char** table) {
  > int coeff = 5;
  > coeff = 2;
  > return coeff;
  > }
  > EOF
  [{ function_type = TInt32; function_name = "main";
     function_arguments =
     [(TInt32, "number"); ((TPointer (TPointer TChar)), "table")];
     function_body =
     (Some (StatementsBlock
              [(Expression
                  (DefineSeq
                     [(Define (TInt32, (Variable "coeff"),
                         (Some (Value (VInt 5l)))))
                       ]));
                (Expression (Assign ((Variable "coeff"), (Value (VInt 2l)))));
                (Return (Variable "coeff"))]))
     }
    ]

  $ ./demoParse.exe <<-EOF
  > int main(int number, char** table) {
  > char* coeff = ToString(number);
  > coeff += 2;
  > coeff = (int*)coeff + 5;
  > Insert(table, coeff);
  > }
  > EOF
  [{ function_type = TInt32; function_name = "main";
     function_arguments =
     [(TInt32, "number"); ((TPointer (TPointer TChar)), "table")];
     function_body =
     (Some (StatementsBlock
              [(Expression
                  (DefineSeq
                     [(Define ((TPointer TChar), (Variable "coeff"),
                         (Some (FuncCall ("ToString", [(Variable "number")])))
                         ))
                       ]));
                (Expression
                   (Assign ((Variable "coeff"),
                      (Add ((Variable "coeff"), (Value (VInt 2l)))))));
                (Expression
                   (Assign ((Variable "coeff"),
                      (Add ((Cast ((TPointer TInt32), (Variable "coeff"))),
                         (Value (VInt 5l))))
                      )));
                (Expression
                   (FuncCall ("Insert",
                      [(Variable "table"); (Variable "coeff")])))
                ]))
     }
    ]

  $ ./demoParse.exe <<-EOF
  > void Helper(int value) {
  >   return value * (value + 1);
  > }
  > int main() {
  >   int n, i;
  >   int fact = 1;
  >   printf("Enter an integer: ");
  >   scanf("%d", &n);
  >   if (n < 0)
  >   {
  >      printf("Error! Factorial of a negative number doesn't exist.");
  >   }
  >   else {
  >     for (i = 1; i <= n; i++) {
  >       fact *= i;
  >     }
  >     printf("Factorial of %d = %llu", n, fact);
  >   }
  >   return 0;
  > }
  > EOF
  [{ function_type = TVoid; function_name = "Helper";
     function_arguments = [(TInt32, "value")];
     function_body =
     (Some (StatementsBlock
              [(Return
                  (Mul ((Variable "value"),
                     (Add ((Variable "value"), (Value (VInt 1l)))))))
                ]))
     };
    { function_type = TInt32; function_name = "main"; function_arguments = [];
      function_body =
      (Some (StatementsBlock
               [(Expression
                   (DefineSeq
                      [(Define (TInt32, (Variable "n"), None));
                        (Define (TInt32, (Variable "i"), None))]));
                 (Expression
                    (DefineSeq
                       [(Define (TInt32, (Variable "fact"),
                           (Some (Value (VInt 1l)))))
                         ]));
                 (Expression
                    (FuncCall ("printf",
                       [(Value (VString "Enter an integer: "))])));
                 (Expression
                    (FuncCall ("scanf",
                       [(Value (VString "%d")); (Address (Variable "n"))])));
                 (IfSeq (
                    [(If ((Less ((Variable "n"), (Value (VInt 0l)))),
                        (StatementsBlock
                           [(Expression
                               (FuncCall ("printf",
                                  [(Value
                                      (VString
                                         "Error! Factorial of a negative number doesn't exist."))
                                    ]
                                  )))
                             ])
                        ))
                      ],
                    (Some (StatementsBlock
                             [(For (
                                 (Some (Assign ((Variable "i"),
                                          (Value (VInt 1l))))),
                                 (Some (LessOrEq ((Variable "i"),
                                          (Variable "n")))),
                                 (Some (Assign ((Variable "i"),
                                          (Add ((Variable "i"),
                                             (Value (VInt 1l))))
                                          ))),
                                 (StatementsBlock
                                    [(Expression
                                        (Assign ((Variable "fact"),
                                           (Mul ((Variable "fact"),
                                              (Variable "i")))
                                           )))
                                      ])
                                 ));
                               (Expression
                                  (FuncCall ("printf",
                                     [(Value (VString "Factorial of %d = %llu"));
                                       (Variable "n"); (Variable "fact")]
                                     )))
                               ]))
                    ));
                 (Return (Value (VInt 0l)))]))
      }
    ]
