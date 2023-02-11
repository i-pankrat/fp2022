  $ (cd ../../../../default && demos/newInterpreterTest.exe)
  -_-_-_-_-_-_-_-_-_-_- New interpretation testing -_-_-_-_-_-_-_-_-_-_-
  
  ######################################
  a to A .F() = 7
  ######################################
  b.F() = 8
  b to A .F() = 8
  b to I .F() = 8
  ######################################
  c.F() = 9
  c to A .F() = 9
  c to B .F() = 9
  c to I .F() = 100
  ######################################
  d.F() = -10
  d to A .F() = 9
  d to B .F() = 9
  d to C .F() = 9
  d to I .F() = 100
  ######################################
  d1.F() = -11
  d1 to A .F() = 9
  d1 to B .F() = 9
  d1 to C .F() = 9
  d1 to I .F() = -11
  ######################################
  e.F() = 10
  e to A .F() = 9
  e to B .F() = 9
  e to C .F() = 9
  e to I .F() = 100
  e to A to I .F() = 100
  e to B to I .F() = 100
  e to C to I .F() = 100
  e to B to A .F() = 9
  e to C to B .F() = 9
  e to C to A .F() = 9
  ######################################
  g.F() = 11
  g to A .F() = 9
  g to B .F() = 9
  g to C .F() = 9
  g to E .F() = 11
  g to I .F() = 11
  ######################################
  h.F() = 19
  h to A .F() = 9
  h to B .F() = 9
  h to C .F() = 9
  h to E .F() = 19
  h to I .F() = 100
  { cur_object =
    ObjectReference ({ class_key = "Program"; ext_interface = None;
                       field_references_table = []; number = 0 });
    var_table =
    ["a": { v_type = TRef ("A"); v_key = "a"; is_const = false;
            assignment_count = 1;
            v_value =
            VObjectReference (ObjectReference ({ class_key = "A";
                                                 ext_interface = None;
                                                 field_references_table = [];
                                                 number = 1 }));
            vis_lvl = 0 },
     "b": { v_type = TRef ("B"); v_key = "b"; is_const = false;
            assignment_count = 1;
            v_value =
            VObjectReference (ObjectReference ({ class_key = "B";
                                                 ext_interface = None;
                                                 field_references_table = [];
                                                 number = 2 }));
            vis_lvl = 0 },
     "c": { v_type = TRef ("C"); v_key = "c"; is_const = false;
            assignment_count = 1;
            v_value =
            VObjectReference (ObjectReference ({ class_key = "C";
                                                 ext_interface = None;
                                                 field_references_table = [];
                                                 number = 3 }));
            vis_lvl = 0 },
     "d": { v_type = TRef ("D"); v_key = "d"; is_const = false;
            assignment_count = 1;
            v_value =
            VObjectReference (ObjectReference ({ class_key = "D";
                                                 ext_interface = None;
                                                 field_references_table = [];
                                                 number = 4 }));
            vis_lvl = 0 },
     "d1": { v_type = TRef ("D1"); v_key = "d1"; is_const = false;
             assignment_count = 1;
             v_value =
             VObjectReference (ObjectReference ({ class_key = "D1";
                                                  ext_interface = None;
                                                  field_references_table = [];
                                                  number = 5 }));
             vis_lvl = 0 },
     "e": { v_type = TRef ("E"); v_key = "e"; is_const = false;
            assignment_count = 1;
            v_value =
            VObjectReference (ObjectReference ({ class_key = "E";
                                                 ext_interface = None;
                                                 field_references_table = [];
                                                 number = 6 }));
            vis_lvl = 0 },
     "g": { v_type = TRef ("G"); v_key = "g"; is_const = false;
            assignment_count = 1;
            v_value =
            VObjectReference (ObjectReference ({ class_key = "G";
                                                 ext_interface = None;
                                                 field_references_table = [];
                                                 number = 7 }));
            vis_lvl = 0 },
     "h": { v_type = TRef ("H"); v_key = "h"; is_const = false;
            assignment_count = 1;
            v_value =
            VObjectReference (ObjectReference ({ class_key = "H";
                                                 ext_interface = None;
                                                 field_references_table = [];
                                                 number = 8 }));
            vis_lvl = 0 },
     ];
    last_expr_result = VVoid; runtime_signal = NoSignal;
    curr_method_type = TVoid; is_main_scope = true; nested_loops_cnt = 0;
    visibility_level = 0; cur_constr_key = None; prev_context = None;
    obj_created_cnt = 8; is_creation = false; constr_affilation = None;
    class_for_cast = None }
  
