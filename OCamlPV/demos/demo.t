Plus 
  $ ./demo.exe <<- EOF
  > let x = 2 + 40
  > EOF
  42
Sum
  $ ./demo.exe <<- EOF
  > let sum x y = x + y in (sum 2 2)
  > EOF
  4
Factorial
  $ ./demo.exe <<- EOF
  > let rec fact n = if n = 1 then 1 else n * (fact (n - 1)) in
  >   (fact 5)
  > EOF
  120
Fibonacci
  $ ./demo.exe <<- EOF
  > let rec fib n = if n <= 1 then 1 else (fib (n - 1)) + (fib (n - 2));;
  > let result = (fib 5)
  > EOF
  8
Sum of the first n elements
  $ ./demo.exe <<- EOF
  > let rec sumn n = if n = 1 then 1 else n + (sumn (n - 1));;
  > let result = (sumn 100)
  5050
List.Rev
  $ ./demo.exe <<- EOF
  > let list_rev list =
  >   let rec helper acc l =
  >     match l with
  >       | [] -> acc
  >       | hd :: tl -> (helper (hd :: acc) tl)
  > in (helper [] list);;
  > let result = (list_rev [1; 2; 3; 4; 5])
  > EOF
  [5; 4; 3; 2; 1]
List.Map
  $ ./demo.exe <<- EOF
  > let rec list_map f list =
  >     match list with
  >       | [] -> []
  >       | hd :: tl -> (f hd) :: (list_map f tl);;
  > let result = (list_map (fun x -> x * x) [1; 2; 3; 4; 5])
  > EOF
  [1; 4; 9; 16; 25]
List.Fold
  $ ./demo.exe <<- EOF
  > let rec list_fold list acc f =
  >   let rec helper l acc = 
  >     match l with
  >       | [] -> acc
  >       | hd :: tl -> (helper tl (f acc hd))
  >   in (helper list acc);;
  > let result = (list_fold [1; 2; 3; 4; 5] 0 (fun acc el -> acc + el))
  > EOF
  15
List.append
  $ ./demo.exe <<- EOF
  > let list_append l1 l2 =
  >   let rec helper l = 
  >     match l with
  >       | [] -> l2
  >       | hd :: tl -> hd :: (helper tl)
  >   in (helper l1);;
  > let result = (list_append [1; 2; 3] [4; 5; 6])
  > EOF
  [1; 2; 3; 4; 5; 6]
List.concat
  $ ./demo.exe <<- EOF
  > let list_concat list =
  >   let rec concat2 l1 l2 = 
  >     match l1 with
  >       | [] -> l2
  >       | hd :: tl -> hd :: (concat2 tl l2) in
  >   let rec helper l = 
  >     match l with
  >       | [] -> []
  >       | hd :: tl -> (concat2 hd (helper tl))
  >   in (helper list);;
  > let result = (list_concat [[1; 2; 3]; [4; 5; 6]])
  > EOF
  [1; 2; 3; 4; 5; 6]
List.filter
  $ ./demo.exe <<- EOF
  > let list_filter list f =
  >   let rec helper l = 
  >     match l with
  >       | [] -> []
  >       | hd :: tl -> if (f hd) then hd :: (helper tl) else (helper tl)
  >   in (helper list);;
  > let result = (list_filter [1; 2; 3; 4; 5; 6; 7] (fun x -> x >= 5))
  > EOF
  [5; 6; 7]
fst
  $ ./demo.exe <<- EOF
  > let fst (f, s) = f;;
  > let result = (fst ("Dmitry", "Kosarev"))
  > EOF
  "Dmitry"
snd
  $ ./demo.exe <<- EOF
  > let snd (f, s) = s;;
  > let result = (snd ("Dmitry", "Kosarev"))
  > EOF
  "Kosarev"
