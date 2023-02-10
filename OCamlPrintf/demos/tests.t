Simple arithmetic
  $ ./demo.exe <<-"EOF"
  > (225 + 20 * 10) / 5
  > EOF
  (VConst (CInt 85))
  $ ./demo.exe <<-"EOF"
  > "str" ^ "str1"
  > EOF
  (VConst (CString "strstr1"))
  $ ./demo.exe <<-"EOF"
  > true = false = false
  > EOF
  (VConst (CBool true))

Let in examples (with application)
  $ ./demo.exe <<-"EOF"
  > let plus5 = (+) 5 in
  >     plus5 10
  > EOF
  (VConst (CInt 15))
  $ ./demo.exe <<-"EOF"
  > let plus = (+) in
  >     plus 1 2
  > EOF
  (VConst (CInt 3))
  $ ./demo.exe <<-"EOF"
  > let id = fun x -> x in
  >     id 5
  > EOF
  (VConst (CInt 5))

Conditional expr
  $ ./demo.exe <<-"EOF"
  > let x = if true then 4 else 1 in
  >     x
  > EOF
  (VConst (CInt 4))
  $ ./demo.exe <<-"EOF"
  > let x = if 1 then 4 else 1 in
  >     x
  > EOF
  Unification failed: type of the expression is 'int' but expected type was 'bool'

Printf examples (print result and returns VUnit)
  $ ./demo.exe <<-"EOF"
  > printf "str"
  > EOF
  str
  (VConst CUnit)
  $ ./demo.exe <<-"EOF"
  > printf "%i" 7
  > EOF
  7
  (VConst CUnit)
  $ ./demo.exe <<-"EOF"
  > printf "%s%s" "str" "str"
  > EOF
  strstr
  (VConst CUnit)

  $ ./demo.exe <<-"EOF"
  > printf "%S%s" "a" "b2"
  > EOF
  'a'b2
  (VConst CUnit)
  $ ./demo.exe <<-"EOF"
  > printf "a%Sb%sc" "A" "B"
  > EOF
  a'A'bBc
  (VConst CUnit)

  $ ./demo.exe <<-"EOF"
  > printf "abcdef%i%s%S" 5 "abc" "def"
  > EOF
  abcdef5abc'def'
  (VConst CUnit)
  $ ./demo.exe <<-"EOF"
  > printf "k%i%s4%S" 4 "k" "du"
  > EOF
  k4k4'du'
  (VConst CUnit)
