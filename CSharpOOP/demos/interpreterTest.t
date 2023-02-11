  $ (cd ../../../../default && demos/interpreterTest.exe)
  -_-_-_-_-_-_-_-_-_-_- Simple interpretation testing -_-_-_-_-_-_-_-_-_-_-
  
  { cur_object =
    ObjectReference ({ class_key = "Program"; ext_interface = None;
                       field_references_table = []; number = 0 });
    var_table =
    ["a": { v_type = TInt; v_key = "a"; is_const = false; assignment_count = 1;
            v_value = VInt (1); vis_lvl = 0 },
     "b": { v_type = TInt; v_key = "b"; is_const = false; assignment_count = 1;
            v_value = VInt (2); vis_lvl = 0 },
     "c": { v_type = TInt; v_key = "c"; is_const = false; assignment_count = 1;
            v_value = VInt (3); vis_lvl = 0 },
     ];
    last_expr_result = VInt (3); runtime_signal = NoSignal;
    curr_method_type = TVoid; is_main_scope = true; nested_loops_cnt = 0;
    visibility_level = 0; cur_constr_key = None; prev_context = None;
    obj_created_cnt = 0; is_creation = false; constr_affilation = None;
    class_for_cast = None }
  
  -_-_-_-_-_-_-_-_-_-_- Arithmetic testing -_-_-_-_-_-_-_-_-_-_-
  
  { cur_object =
    ObjectReference ({ class_key = "Program"; ext_interface = None;
                       field_references_table = []; number = 0 });
    var_table =
    ["a": { v_type = TInt; v_key = "a"; is_const = false; assignment_count = 2;
            v_value = VInt (2); vis_lvl = 0 },
     "b": { v_type = TInt; v_key = "b"; is_const = false; assignment_count = 1;
            v_value = VInt (2); vis_lvl = 0 },
     "c": { v_type = TInt; v_key = "c"; is_const = false; assignment_count = 1;
            v_value = VInt (3); vis_lvl = 0 },
     "s1": { v_type = TString; v_key = "s1"; is_const = false;
             assignment_count = 1; v_value = VString ("a"); vis_lvl = 0 },
     "s2": { v_type = TString; v_key = "s2"; is_const = false;
             assignment_count = 1; v_value = VString ("b"); vis_lvl = 0 },
     "s3": { v_type = TString; v_key = "s3"; is_const = false;
             assignment_count = 1; v_value = VString ("ab"); vis_lvl = 0 },
     "s4": { v_type = TString; v_key = "s4"; is_const = false;
             assignment_count = 1; v_value = VString ("a2"); vis_lvl = 0 },
     "s5": { v_type = TString; v_key = "s5"; is_const = false;
             assignment_count = 1; v_value = VString ("2b"); vis_lvl = 0 },
     "val1": { v_type = TInt; v_key = "val1"; is_const = false;
               assignment_count = 1; v_value = VInt (15); vis_lvl = 0 },
     "val2": { v_type = TInt; v_key = "val2"; is_const = false;
               assignment_count = 1; v_value = VInt (3); vis_lvl = 0 },
     "val3": { v_type = TInt; v_key = "val3"; is_const = false;
               assignment_count = 1; v_value = VInt (101); vis_lvl = 0 },
     "val4": { v_type = TInt; v_key = "val4"; is_const = false;
               assignment_count = 1; v_value = VInt (5); vis_lvl = 0 },
     "val5": { v_type = TInt; v_key = "val5"; is_const = false;
               assignment_count = 1; v_value = VInt (0); vis_lvl = 0 },
     "val6": { v_type = TInt; v_key = "val6"; is_const = false;
               assignment_count = 1; v_value = VInt (300); vis_lvl = 0 },
     "val7": { v_type = TInt; v_key = "val7"; is_const = false;
               assignment_count = 1; v_value = VInt (124); vis_lvl = 0 },
     ];
    last_expr_result = VString ("2b"); runtime_signal = NoSignal;
    curr_method_type = TVoid; is_main_scope = true; nested_loops_cnt = 0;
    visibility_level = 0; cur_constr_key = None; prev_context = None;
    obj_created_cnt = 0; is_creation = false; constr_affilation = None;
    class_for_cast = None }
  
  -_-_-_-_-_-_-_-_-_-_- Bool expressions testing -_-_-_-_-_-_-_-_-_-_-
  
  b greater than a
  a less than c
  b less or equal than c
  c greater or equal than b
  d equal to a
  b not equal to a
  a less than b and b less than c
  a less than b or a not equal to 10
  not (a greater or equal to c)
  s1 equal to 'a'
  s2 not equal to 'a'
  p1 not equal to p2
  p1 equal to p3
  { cur_object =
    ObjectReference ({ class_key = "Program"; ext_interface = None;
                       field_references_table = []; number = 0 });
    var_table =
    ["a": { v_type = TInt; v_key = "a"; is_const = false; assignment_count = 1;
            v_value = VInt (10); vis_lvl = 0 },
     "b": { v_type = TInt; v_key = "b"; is_const = false; assignment_count = 1;
            v_value = VInt (50); vis_lvl = 0 },
     "c": { v_type = TInt; v_key = "c"; is_const = false; assignment_count = 1;
            v_value = VInt (100); vis_lvl = 0 },
     "d": { v_type = TInt; v_key = "d"; is_const = false; assignment_count = 1;
            v_value = VInt (10); vis_lvl = 0 },
     "p1": { v_type = TRef ("Person"); v_key = "p1"; is_const = false;
             assignment_count = 1;
             v_value =
             VObjectReference (ObjectReference ({ class_key = "Person";
                                                  ext_interface = None;
                                                  field_references_table =
                                                  ["age": { key = "age";
                                                            field_type = TInt;
                                                            field_value =
                                                            VInt (20);
                                                            is_const = false;
                                                            assignments_count =
                                                            0 },
                                                   "name": { key = "name";
                                                             field_type =
                                                             TString;
                                                             field_value =
                                                             VString ("Bob");
                                                             is_const = false;
                                                             assignments_count =
                                                             0 },
                                                   ];
                                                  number = 1 }));
             vis_lvl = 0 },
     "p2": { v_type = TRef ("Person"); v_key = "p2"; is_const = false;
             assignment_count = 1;
             v_value =
             VObjectReference (ObjectReference ({ class_key = "Person";
                                                  ext_interface = None;
                                                  field_references_table =
                                                  ["age": { key = "age";
                                                            field_type = TInt;
                                                            field_value =
                                                            VInt (30);
                                                            is_const = false;
                                                            assignments_count =
                                                            0 },
                                                   "name": { key = "name";
                                                             field_type =
                                                             TString;
                                                             field_value =
                                                             VString ("Alice");
                                                             is_const = false;
                                                             assignments_count =
                                                             0 },
                                                   ];
                                                  number = 2 }));
             vis_lvl = 0 },
     "p3": { v_type = TRef ("Person"); v_key = "p3"; is_const = false;
             assignment_count = 1;
             v_value =
             VObjectReference (ObjectReference ({ class_key = "Person";
                                                  ext_interface = None;
                                                  field_references_table =
                                                  ["age": { key = "age";
                                                            field_type = TInt;
                                                            field_value =
                                                            VInt (20);
                                                            is_const = false;
                                                            assignments_count =
                                                            0 },
                                                   "name": { key = "name";
                                                             field_type =
                                                             TString;
                                                             field_value =
                                                             VString ("Bob");
                                                             is_const = false;
                                                             assignments_count =
                                                             0 },
                                                   ];
                                                  number = 1 }));
             vis_lvl = 0 },
     "s1": { v_type = TString; v_key = "s1"; is_const = false;
             assignment_count = 1; v_value = VString ("a"); vis_lvl = 0 },
     "s2": { v_type = TString; v_key = "s2"; is_const = false;
             assignment_count = 1; v_value = VString ("b"); vis_lvl = 0 },
     ];
    last_expr_result = VVoid; runtime_signal = NoSignal;
    curr_method_type = TVoid; is_main_scope = true; nested_loops_cnt = 0;
    visibility_level = 0; cur_constr_key = None; prev_context = None;
    obj_created_cnt = 2; is_creation = false; constr_affilation = None;
    class_for_cast = None }
  
  -_-_-_-_-_-_-_-_-_-_- Method call testing -_-_-_-_-_-_-_-_-_-_-
  
  Old age = 25
  New age = 30
  { cur_object =
    ObjectReference ({ class_key = "Program"; ext_interface = None;
                       field_references_table = []; number = 0 });
    var_table =
    ["a1": { v_type = TInt; v_key = "a1"; is_const = false;
             assignment_count = 1; v_value = VInt (25); vis_lvl = 0 },
     "a2": { v_type = TInt; v_key = "a2"; is_const = false;
             assignment_count = 1; v_value = VInt (30); vis_lvl = 0 },
     "person": { v_type = TRef ("Person"); v_key = "person"; is_const = false;
                 assignment_count = 1;
                 v_value =
                 VObjectReference (ObjectReference ({ class_key = "Person";
                                                      ext_interface = None;
                                                      field_references_table =
                                                      ["age": { key = "age";
                                                                field_type =
                                                                TInt;
                                                                field_value =
                                                                VInt (30);
                                                                is_const =
                                                                false;
                                                                assignments_count =
                                                                1 },
                                                       "name": { key = "name";
                                                                 field_type =
                                                                 TString;
                                                                 field_value =
                                                                 VString ("Bob");
                                                                 is_const =
                                                                 false;
                                                                 assignments_count =
                                                                 0 },
                                                       ];
                                                      number = 1 }));
                 vis_lvl = 0 },
     "res": { v_type = TInt; v_key = "res"; is_const = false;
              assignment_count = 1; v_value = VInt (125); vis_lvl = 0 },
     ];
    last_expr_result = VVoid; runtime_signal = NoSignal;
    curr_method_type = TVoid; is_main_scope = true; nested_loops_cnt = 0;
    visibility_level = 0; cur_constr_key = None; prev_context = None;
    obj_created_cnt = 1; is_creation = false; constr_affilation = None;
    class_for_cast = None }
  
  -_-_-_-_-_-_-_-_-_-_- Update object state testing -_-_-_-_-_-_-_-_-_-_-
  
  55
  { cur_object =
    ObjectReference ({ class_key = "Program"; ext_interface = None;
                       field_references_table = []; number = 0 });
    var_table =
    ["p1": { v_type = TRef ("Person"); v_key = "p1"; is_const = false;
             assignment_count = 1;
             v_value =
             VObjectReference (ObjectReference ({ class_key = "Person";
                                                  ext_interface = None;
                                                  field_references_table =
                                                  ["age": { key = "age";
                                                            field_type = TInt;
                                                            field_value =
                                                            VInt (55);
                                                            is_const = false;
                                                            assignments_count =
                                                            1 },
                                                   "name": { key = "name";
                                                             field_type =
                                                             TString;
                                                             field_value =
                                                             VString ("Bob");
                                                             is_const = false;
                                                             assignments_count =
                                                             0 },
                                                   ];
                                                  number = 1 }));
             vis_lvl = 0 },
     "p2": { v_type = TRef ("Person"); v_key = "p2"; is_const = false;
             assignment_count = 1;
             v_value =
             VObjectReference (ObjectReference ({ class_key = "Person";
                                                  ext_interface = None;
                                                  field_references_table =
                                                  ["age": { key = "age";
                                                            field_type = TInt;
                                                            field_value =
                                                            VInt (55);
                                                            is_const = false;
                                                            assignments_count =
                                                            1 },
                                                   "name": { key = "name";
                                                             field_type =
                                                             TString;
                                                             field_value =
                                                             VString ("Bob");
                                                             is_const = false;
                                                             assignments_count =
                                                             0 },
                                                   ];
                                                  number = 1 }));
             vis_lvl = 0 },
     "p3": { v_type = TRef ("Person"); v_key = "p3"; is_const = false;
             assignment_count = 1;
             v_value =
             VObjectReference (ObjectReference ({ class_key = "Person";
                                                  ext_interface = None;
                                                  field_references_table =
                                                  ["age": { key = "age";
                                                            field_type = TInt;
                                                            field_value =
                                                            VInt (55);
                                                            is_const = false;
                                                            assignments_count =
                                                            1 },
                                                   "name": { key = "name";
                                                             field_type =
                                                             TString;
                                                             field_value =
                                                             VString ("Bob");
                                                             is_const = false;
                                                             assignments_count =
                                                             0 },
                                                   ];
                                                  number = 1 }));
             vis_lvl = 0 },
     "person": { v_type = TRef ("Person"); v_key = "person"; is_const = false;
                 assignment_count = 1;
                 v_value =
                 VObjectReference (ObjectReference ({ class_key = "Person";
                                                      ext_interface = None;
                                                      field_references_table =
                                                      ["age": { key = "age";
                                                                field_type =
                                                                TInt;
                                                                field_value =
                                                                VInt (55);
                                                                is_const =
                                                                false;
                                                                assignments_count =
                                                                1 },
                                                       "name": { key = "name";
                                                                 field_type =
                                                                 TString;
                                                                 field_value =
                                                                 VString ("Bob");
                                                                 is_const =
                                                                 false;
                                                                 assignments_count =
                                                                 0 },
                                                       ];
                                                      number = 1 }));
                 vis_lvl = 0 },
     "res": { v_type = TInt; v_key = "res"; is_const = false;
              assignment_count = 1; v_value = VInt (55); vis_lvl = 0 },
     ];
    last_expr_result = VVoid; runtime_signal = NoSignal;
    curr_method_type = TVoid; is_main_scope = true; nested_loops_cnt = 0;
    visibility_level = 0; cur_constr_key = None; prev_context = None;
    obj_created_cnt = 1; is_creation = false; constr_affilation = None;
    class_for_cast = None }
  
  -_-_-_-_-_-_-_-_-_-_- Inheritance testing -_-_-_-_-_-_-_-_-_-_-
  
  Child second default age = 0
  Child second new age = 20
  Parent new age = 27
  Child first new age = 4
  Child second new parent age  = 20
  { cur_object =
    ObjectReference ({ class_key = "Program"; ext_interface = None;
                       field_references_table = []; number = 0 });
    var_table =
    ["childFirst": { v_type = TRef ("Child"); v_key = "childFirst";
                     is_const = false; assignment_count = 1;
                     v_value =
                     VObjectReference (ObjectReference ({ class_key = "Child";
                                                          ext_interface =
                                                          Some ("Person");
                                                          field_references_table =
                                                          ["age": { key = "age";
                                                                    field_type =
                                                                    TInt;
                                                                    field_value =
                                                                    VInt (4);
                                                                    is_const =
                                                                    false;
                                                                    assignments_count =
                                                                    1 },
                                                           "name": { key =
                                                                     "name";
                                                                     field_type =
                                                                     TString;
                                                                     field_value =
                                                                     VString ("Alice");
                                                                     is_const =
                                                                     false;
                                                                     assignments_count =
                                                                     0 },
                                                           "parent": { key =
                                                                      "parent";
                                                                      field_type =
                                                                      TRef ("Person");
                                                                      field_value =
                                                                      VObjectReference (
                                                                      ObjectReference (
                                                                      { class_key =
                                                                      "Person";
                                                                      ext_interface =
                                                                      None;
                                                                      field_references_table =
                                                                      [
                                                                      "age": 
                                                                      { key =
                                                                      "age";
                                                                      field_type =
                                                                      TInt;
                                                                      field_value =
                                                                      VInt (40);
                                                                      is_const =
                                                                      false;
                                                                      assignments_count =
                                                                      0 },
                                                                      "name": 
                                                                      { key =
                                                                      "name";
                                                                      field_type =
                                                                      TString;
                                                                      field_value =
                                                                      VString ("Spike");
                                                                      is_const =
                                                                      false;
                                                                      assignments_count =
                                                                      0 },
                                                                      ];
                                                                      number =
                                                                      3 }));
                                                                      is_const =
                                                                      false;
                                                                      assignments_count =
                                                                      0 },
                                                           ];
                                                          number = 2 }));
                     vis_lvl = 0 },
     "childSecond": { v_type = TRef ("Child"); v_key = "childSecond";
                      is_const = false; assignment_count = 1;
                      v_value =
                      VObjectReference (ObjectReference ({ class_key = "Child";
                                                           ext_interface = None;
                                                           field_references_table =
                                                           ["age": { key =
                                                                     "age";
                                                                     field_type =
                                                                     TInt;
                                                                     field_value =
                                                                     VInt (20);
                                                                     is_const =
                                                                     false;
                                                                     assignments_count =
                                                                     1 },
                                                            "name": { key =
                                                                      "name";
                                                                      field_type =
                                                                      TString;
                                                                      field_value =
                                                                      VString ("");
                                                                      is_const =
                                                                      false;
                                                                      assignments_count =
                                                                      0 },
                                                            "parent": { key =
                                                                      "parent";
                                                                      field_type =
                                                                      TRef ("Person");
                                                                      field_value =
                                                                      VObjectReference (
                                                                      ObjectReference (
                                                                      { class_key =
                                                                      "Person";
                                                                      ext_interface =
                                                                      None;
                                                                      field_references_table =
                                                                      [
                                                                      "age": 
                                                                      { key =
                                                                      "age";
                                                                      field_type =
                                                                      TInt;
                                                                      field_value =
                                                                      VInt (27);
                                                                      is_const =
                                                                      false;
                                                                      assignments_count =
                                                                      1 },
                                                                      "name": 
                                                                      { key =
                                                                      "name";
                                                                      field_type =
                                                                      TString;
                                                                      field_value =
                                                                      VString ("Bob");
                                                                      is_const =
                                                                      false;
                                                                      assignments_count =
                                                                      0 },
                                                                      ];
                                                                      number =
                                                                      1 }));
                                                                      is_const =
                                                                      false;
                                                                      assignments_count =
                                                                      0 },
                                                            ];
                                                           number = 4 }));
                      vis_lvl = 0 },
     "person": { v_type = TRef ("Person"); v_key = "person"; is_const = false;
                 assignment_count = 1;
                 v_value =
                 VObjectReference (ObjectReference ({ class_key = "Person";
                                                      ext_interface = None;
                                                      field_references_table =
                                                      ["age": { key = "age";
                                                                field_type =
                                                                TInt;
                                                                field_value =
                                                                VInt (27);
                                                                is_const =
                                                                false;
                                                                assignments_count =
                                                                1 },
                                                       "name": { key = "name";
                                                                 field_type =
                                                                 TString;
                                                                 field_value =
                                                                 VString ("Bob");
                                                                 is_const =
                                                                 false;
                                                                 assignments_count =
                                                                 0 },
                                                       ];
                                                      number = 1 }));
                 vis_lvl = 0 },
     ];
    last_expr_result = VVoid; runtime_signal = NoSignal;
    curr_method_type = TVoid; is_main_scope = true; nested_loops_cnt = 0;
    visibility_level = 0; cur_constr_key = None; prev_context = None;
    obj_created_cnt = 4; is_creation = false; constr_affilation = None;
    class_for_cast = None }
  
  -_-_-_-_-_-_-_-_-_-_- Scope testing -_-_-_-_-_-_-_-_-_-_-
  
  { cur_object =
    ObjectReference ({ class_key = "Program"; ext_interface = None;
                       field_references_table = []; number = 0 });
    var_table =
    ["a": { v_type = TInt; v_key = "a"; is_const = false; assignment_count = 5;
            v_value = VInt (1000); vis_lvl = 0 },
     "b": { v_type = TInt; v_key = "b"; is_const = false; assignment_count = 4;
            v_value = VInt (2000); vis_lvl = 0 },
     "c": { v_type = TInt; v_key = "c"; is_const = false; assignment_count = 4;
            v_value = VInt (3000); vis_lvl = 0 },
     "i": { v_type = TInt; v_key = "i"; is_const = false; assignment_count = 4;
            v_value = VInt (3); vis_lvl = 0 },
     ];
    last_expr_result = VInt (3000); runtime_signal = NoSignal;
    curr_method_type = TVoid; is_main_scope = true; nested_loops_cnt = 0;
    visibility_level = 1; cur_constr_key = None; prev_context = None;
    obj_created_cnt = 0; is_creation = false; constr_affilation = None;
    class_for_cast = None }
  
  -_-_-_-_-_-_-_-_-_-_- Many loops testing -_-_-_-_-_-_-_-_-_-_-
  
  temp = 1
  temp = 2
  { cur_object =
    ObjectReference ({ class_key = "Program"; ext_interface = None;
                       field_references_table = []; number = 0 });
    var_table =
    ["i": { v_type = TInt; v_key = "i"; is_const = false; assignment_count = 4;
            v_value = VInt (3); vis_lvl = 1 },
     "n": { v_type = TInt; v_key = "n"; is_const = false; assignment_count = 1;
            v_value = VInt (4); vis_lvl = 0 },
     ];
    last_expr_result = VBool (false); runtime_signal = NoSignal;
    curr_method_type = TVoid; is_main_scope = true; nested_loops_cnt = 0;
    visibility_level = 2; cur_constr_key = None; prev_context = None;
    obj_created_cnt = 0; is_creation = false; constr_affilation = None;
    class_for_cast = None }
  
  -_-_-_-_-_-_-_-_-_-_- Break and continue testing -_-_-_-_-_-_-_-_-_-_-
  
  i = 1
  i = 3
  i = 5
  i = 7
  i = 9
  #####
  i = 0
  i = 1
  i = 2
  i = 3
  i = 4
  i = 5
  { cur_object =
    ObjectReference ({ class_key = "Program"; ext_interface = None;
                       field_references_table = []; number = 0 });
    var_table = []; last_expr_result = VInt (6); runtime_signal = NoSignal;
    curr_method_type = TVoid; is_main_scope = true; nested_loops_cnt = 0;
    visibility_level = 2; cur_constr_key = None; prev_context = None;
    obj_created_cnt = 0; is_creation = false; constr_affilation = None;
    class_for_cast = None }
  
  -_-_-_-_-_-_-_-_-_-_- Object state changing in another context testing -_-_-_-_-_-_-_-_-_-_-
  
  30
  { cur_object =
    ObjectReference ({ class_key = "Program"; ext_interface = None;
                       field_references_table = []; number = 0 });
    var_table =
    ["child": { v_type = TRef ("Child"); v_key = "child"; is_const = false;
                assignment_count = 1;
                v_value =
                VObjectReference (ObjectReference ({ class_key = "Child";
                                                     ext_interface = None;
                                                     field_references_table =
                                                     ["age": { key = "age";
                                                               field_type =
                                                               TInt;
                                                               field_value =
                                                               VInt (0);
                                                               is_const = false;
                                                               assignments_count =
                                                               0 },
                                                      "name": { key = "name";
                                                                field_type =
                                                                TString;
                                                                field_value =
                                                                VString ("");
                                                                is_const =
                                                                false;
                                                                assignments_count =
                                                                0 },
                                                      "parent": { key =
                                                                  "parent";
                                                                  field_type =
                                                                  TRef ("Person");
                                                                  field_value =
                                                                  VObjectReference (
                                                                   ObjectReference (
                                                                    { class_key =
                                                                      "Person";
                                                                      ext_interface =
                                                                      None;
                                                                      field_references_table =
                                                                      [
                                                                      "age": 
                                                                      { key =
                                                                      "age";
                                                                      field_type =
                                                                      TInt;
                                                                      field_value =
                                                                      VInt (30);
                                                                      is_const =
                                                                      false;
                                                                      assignments_count =
                                                                      1 },
                                                                      "name": 
                                                                      { key =
                                                                      "name";
                                                                      field_type =
                                                                      TString;
                                                                      field_value =
                                                                      VString ("Bob");
                                                                      is_const =
                                                                      false;
                                                                      assignments_count =
                                                                      0 },
                                                                      ];
                                                                      number =
                                                                      1 }));
                                                                  is_const =
                                                                  false;
                                                                  assignments_count =
                                                                  0 },
                                                      ];
                                                     number = 2 }));
                vis_lvl = 0 },
     "person": { v_type = TRef ("Person"); v_key = "person"; is_const = false;
                 assignment_count = 1;
                 v_value =
                 VObjectReference (ObjectReference ({ class_key = "Person";
                                                      ext_interface = None;
                                                      field_references_table =
                                                      ["age": { key = "age";
                                                                field_type =
                                                                TInt;
                                                                field_value =
                                                                VInt (30);
                                                                is_const =
                                                                false;
                                                                assignments_count =
                                                                1 },
                                                       "name": { key = "name";
                                                                 field_type =
                                                                 TString;
                                                                 field_value =
                                                                 VString ("Bob");
                                                                 is_const =
                                                                 false;
                                                                 assignments_count =
                                                                 0 },
                                                       ];
                                                      number = 1 }));
                 vis_lvl = 0 },
     ];
    last_expr_result = VVoid; runtime_signal = NoSignal;
    curr_method_type = TVoid; is_main_scope = true; nested_loops_cnt = 0;
    visibility_level = 0; cur_constr_key = None; prev_context = None;
    obj_created_cnt = 2; is_creation = false; constr_affilation = None;
    class_for_cast = None }
  
  -_-_-_-_-_-_-_-_-_-_- Factorial recursion testing -_-_-_-_-_-_-_-_-_-_-
  
  120
  120
  { cur_object =
    ObjectReference ({ class_key = "Program"; ext_interface = None;
                       field_references_table = []; number = 0 });
    var_table =
    ["f": { v_type = TInt; v_key = "f"; is_const = false; assignment_count = 1;
            v_value = VInt (120); vis_lvl = 0 },
     "factorial": { v_type = TRef ("Factorial"); v_key = "factorial";
                    is_const = false; assignment_count = 1;
                    v_value =
                    VObjectReference (ObjectReference ({ class_key =
                                                         "Factorial";
                                                         ext_interface = None;
                                                         field_references_table =
                                                         []; number = 1 }));
                    vis_lvl = 0 },
     ];
    last_expr_result = VVoid; runtime_signal = NoSignal;
    curr_method_type = TVoid; is_main_scope = true; nested_loops_cnt = 0;
    visibility_level = 0; cur_constr_key = None; prev_context = None;
    obj_created_cnt = 1; is_creation = false; constr_affilation = None;
    class_for_cast = None }
  
  -_-_-_-_-_-_-_-_-_-_- Constructor chaining testing -_-_-_-_-_-_-_-_-_-_-
  
  { cur_object =
    ObjectReference ({ class_key = "Program"; ext_interface = None;
                       field_references_table = []; number = 0 });
    var_table =
    ["cat": { v_type = TRef ("Cat"); v_key = "cat"; is_const = false;
              assignment_count = 1;
              v_value =
              VObjectReference (ObjectReference ({ class_key = "Cat";
                                                   ext_interface = None;
                                                   field_references_table =
                                                   ["age": { key = "age";
                                                             field_type = TInt;
                                                             field_value =
                                                             VInt (2);
                                                             is_const = false;
                                                             assignments_count =
                                                             0 },
                                                    "hairLevel": { key =
                                                                   "hairLevel";
                                                                   field_type =
                                                                   TInt;
                                                                   field_value =
                                                                   VInt (30);
                                                                   is_const =
                                                                   false;
                                                                   assignments_count =
                                                                   0 },
                                                    "name": { key = "name";
                                                              field_type =
                                                              TString;
                                                              field_value =
                                                              VString ("Mars");
                                                              is_const = false;
                                                              assignments_count =
                                                              0 },
                                                    ];
                                                   number = 1 }));
              vis_lvl = 0 },
     ];
    last_expr_result =
    VObjectReference (ObjectReference ({ class_key = "Cat";
                                         ext_interface = None;
                                         field_references_table =
                                         ["age": { key = "age";
                                                   field_type = TInt;
                                                   field_value = VInt (2);
                                                   is_const = false;
                                                   assignments_count = 0 },
                                          "hairLevel": { key = "hairLevel";
                                                         field_type = TInt;
                                                         field_value =
                                                         VInt (30);
                                                         is_const = false;
                                                         assignments_count = 0
                                                         },
                                          "name": { key = "name";
                                                    field_type = TString;
                                                    field_value =
                                                    VString ("Mars");
                                                    is_const = false;
                                                    assignments_count = 0 },
                                          ];
                                         number = 1 }));
    runtime_signal = NoSignal; curr_method_type = TVoid; is_main_scope = true;
    nested_loops_cnt = 0; visibility_level = 0; cur_constr_key = None;
    prev_context = None; obj_created_cnt = 1; is_creation = false;
    constr_affilation = None; class_for_cast = None }
  
  -_-_-_-_-_-_-_-_-_-_- Constructor chaining recursion testing -_-_-_-_-_-_-_-_-_-_-
  
  Constructor recursion
  -_-_-_-_-_-_-_-_-_-_- Const fields testing -_-_-_-_-_-_-_-_-_-_-
  
  Assignment to a constant field
  -_-_-_-_-_-_-_-_-_-_- Const variables testing -_-_-_-_-_-_-_-_-_-_-
  
  Assignment to a constant variable
  -_-_-_-_-_-_-_-_-_-_- Ad-hoc polymorphism, specifically methods overloading, testing -_-_-_-_-_-_-_-_-_-_-
  
  5 + 3 = 8
  F + P = FP
  { cur_object =
    ObjectReference ({ class_key = "Program"; ext_interface = None;
                       field_references_table = []; number = 0 });
    var_table =
    ["a": { v_type = TInt; v_key = "a"; is_const = false; assignment_count = 1;
            v_value = VInt (8); vis_lvl = 0 },
     "s": { v_type = TString; v_key = "s"; is_const = false;
            assignment_count = 1; v_value = VString ("FP"); vis_lvl = 0 },
     "summator": { v_type = TRef ("Summator"); v_key = "summator";
                   is_const = false; assignment_count = 1;
                   v_value =
                   VObjectReference (ObjectReference ({ class_key = "Summator";
                                                        ext_interface = None;
                                                        field_references_table =
                                                        []; number = 1 }));
                   vis_lvl = 0 },
     ];
    last_expr_result = VVoid; runtime_signal = NoSignal;
    curr_method_type = TVoid; is_main_scope = true; nested_loops_cnt = 0;
    visibility_level = 0; cur_constr_key = None; prev_context = None;
    obj_created_cnt = 1; is_creation = false; constr_affilation = None;
    class_for_cast = None }
  
  -_-_-_-_-_-_-_-_-_-_- Cast test -_-_-_-_-_-_-_-_-_-_-
  
  123
  { cur_object =
    ObjectReference ({ class_key = "Program"; ext_interface = None;
                       field_references_table = []; number = 0 });
    var_table =
    ["a": { v_type = TInt; v_key = "a"; is_const = false; assignment_count = 1;
            v_value = VInt (123); vis_lvl = 0 },
     "b": { v_type = TRef ("SomeClass"); v_key = "b"; is_const = false;
            assignment_count = 1;
            v_value =
            VObjectReference (ObjectReference ({ class_key = "SomeClass";
                                                 ext_interface = None;
                                                 field_references_table = [];
                                                 number = 1 }));
            vis_lvl = 0 },
     ];
    last_expr_result = VVoid; runtime_signal = NoSignal;
    curr_method_type = TVoid; is_main_scope = true; nested_loops_cnt = 0;
    visibility_level = 0; cur_constr_key = None; prev_context = None;
    obj_created_cnt = 1; is_creation = false; constr_affilation = None;
    class_for_cast = None }
  
  -_-_-_-_-_-_-_-_-_-_- Cast value test -_-_-_-_-_-_-_-_-_-_-
  
  1
  true
  { cur_object =
    ObjectReference ({ class_key = "Program"; ext_interface = None;
                       field_references_table = []; number = 0 });
    var_table =
    ["a": { v_type = TInt; v_key = "a"; is_const = false; assignment_count = 1;
            v_value = VInt (1); vis_lvl = 0 },
     "b": { v_type = TBool; v_key = "b"; is_const = false;
            assignment_count = 1; v_value = VBool (true); vis_lvl = 0 },
     ];
    last_expr_result = VVoid; runtime_signal = NoSignal;
    curr_method_type = TVoid; is_main_scope = true; nested_loops_cnt = 0;
    visibility_level = 0; cur_constr_key = None; prev_context = None;
    obj_created_cnt = 0; is_creation = false; constr_affilation = None;
    class_for_cast = None }
  
  -_-_-_-_-_-_-_-_-_-_- Cast variable test -_-_-_-_-_-_-_-_-_-_-
  
  1
  false
  1
  { cur_object =
    ObjectReference ({ class_key = "Program"; ext_interface = None;
                       field_references_table = []; number = 0 });
    var_table =
    ["a": { v_type = TInt; v_key = "a"; is_const = false; assignment_count = 1;
            v_value = VInt (1); vis_lvl = 0 },
     "b": { v_type = TBool; v_key = "b"; is_const = false;
            assignment_count = 1; v_value = VBool (false); vis_lvl = 0 },
     "c": { v_type = TInt; v_key = "c"; is_const = false; assignment_count = 1;
            v_value = VInt (1); vis_lvl = 0 },
     ];
    last_expr_result = VVoid; runtime_signal = NoSignal;
    curr_method_type = TVoid; is_main_scope = true; nested_loops_cnt = 0;
    visibility_level = 0; cur_constr_key = None; prev_context = None;
    obj_created_cnt = 0; is_creation = false; constr_affilation = None;
    class_for_cast = None }
  
  
  -_-_-_-_-_-_-_-_-_-_- Cast to class -_-_-_-_-_-_-_-_-_-_-
  
  123
  123
  123
  123
  { cur_object =
    ObjectReference ({ class_key = "Program"; ext_interface = None;
                       field_references_table = []; number = 0 });
    var_table =
    ["a": { v_type = TRef ("SomeClass"); v_key = "a"; is_const = false;
            assignment_count = 2;
            v_value =
            VObjectReference (ObjectReference ({ class_key = "SomeClass";
                                                 ext_interface =
                                                 Some ("SomeClass");
                                                 field_references_table = [];
                                                 number = 1 }));
            vis_lvl = 0 },
     "b": { v_type = TRef ("SomeClass"); v_key = "b"; is_const = false;
            assignment_count = 1;
            v_value =
            VObjectReference (ObjectReference ({ class_key = "SomeClass";
                                                 ext_interface = None;
                                                 field_references_table = [];
                                                 number = 1 }));
            vis_lvl = 0 },
     "c": { v_type = TInt; v_key = "c"; is_const = false; assignment_count = 1;
            v_value = VInt (123); vis_lvl = 0 },
     ];
    last_expr_result = VVoid; runtime_signal = NoSignal;
    curr_method_type = TVoid; is_main_scope = true; nested_loops_cnt = 0;
    visibility_level = 0; cur_constr_key = None; prev_context = None;
    obj_created_cnt = 1; is_creation = false; constr_affilation = None;
    class_for_cast = None }
  
  
  -_-_-_-_-_-_-_-_-_-_- Check access to non-visible members -_-_-_-_-_-_-_-_-_-_-
  
  a.f() = 123
  b.f() = 123
  b.g() = 321
  c.f() = 123
  Parent class (interface) A not have G for B class.
  
  -_-_-_-_-_-_-_-_-_-_- Check access modifiers -_-_-_-_-_-_-_-_-_-_-
  
  Cannot access to A.F()
