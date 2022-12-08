(* 

* PUT YOUR NAME HERE                 <--- so we know who you are
* PUT YOUR UserID HERE               <--- so we know who you are
* PUT YOUR CAMPUS HERE               <--- Edinburgh, Dubai, or Malaysia 
* F28PL Coursework 2, OCaml          <--- leave this line unchanged 

* Deadline is *** TBA *** 2022 at 15:30, local time for your campus (Edinburgh / Dubai / Malaysia).

* It is not your marker's role to debug basic syntax errors.
* Therefore, if your script won't compile then it might not be marked.
* In other words: if `ocaml_cw.ml` won't execute, then your marker is not obliged to mark your answers. 

* To do this coursework, FORK, THEN CLONE the gitlab project.
* DO NOT change the name of your fork of the repository.  Your repository must be called 
* f28pl-2022-23-ocaml-coursework. If it is called something else then your markers can't 
* see it and accordingly it cannot be marked.

 *)

(* This is an executable OCaml file *)

(* Answer the questions below by filling in function definitions where required, and writing essay answers in 
   (* comments *) where required. 

   Answer scripts that are not executable may not be marked.
   Code answers that are enclosed in comments may not be marked.
*)

(*

   The marking scheme given below is indicative and may be be adapted to circumstances. 

   An additional (5 marks) total may be awarded for code quality: 
   clarity
   comments
   additional test cases
   finding bugs
   etc.

   You would do well to comment your code to explain your thinking, since even where there are 
   no marks directly for comments, if your thought processes are clear then this may help award 
   marks for understanding.

   Finally: we hope the exercises below are clear, but if in doubt -- ask.
*)

exception Question of string

let cw_fail q = raise (Question q) 

(* ****************************** Start of Questions ********************************* *)

(* (10 marks total) Question 1: functions over boolean lists *)

(* write a function to compute the disjunction of a list of booleans *)

(* (5 marks) Q1a: using direct recursion *)

(* The following code recursively goes through a list and returns true
   if any of the elements of the list at all are true. It does this using
   pattern matching. If the list is empty, it does not have a 'true' in it,
   and therefore will return false. *)

let rec or_list (bs : bool list) = match bs with
  | []  -> false
  | h::t  ->  h || (or_list t);;

let test_q1a_1 = (or_list [] = false);;
let test_q1a_2 = (or_list [false] = false);;
let test_q1a_3 = (or_list [true] = true);;
let test_q1a_4 = (or_list [true;false] = true);;
let test_q1a_5 = (or_list [false;true] = true);;
let test_q1a_6 = (or_list [false;false] = false);;

(* (5 marks) Q1b: using List.fold_right *)

(* The following code folds right through a list using a function that computes
   the disjunction of 2 elements in the list, and then computes the result of that
   disjunction to the next element in the list until the list has been iterated through. *)

let or_list_fold (bs : bool list) = List.fold_right (fun x y  ->  x || y) bs false;;

let test_q1b_1 = (or_list_fold [] = false);;
let test_q1b_2 = (or_list_fold [false] = false);;
let test_q1b_3 = (or_list_fold [true] = true);;
let test_q1b_4 = (or_list_fold [true;false] = true);;
let test_q1b_5 = (or_list_fold [false;true] = true);;
let test_q1b_6 = (or_list_fold [false;false] = false);;

(* ****************************** End of Question 1 ********************************* *)


(* (15 marks total) Question 2: the Sudan function *)

(* Look up the definition of the Sudan function in Wikipedia:
   https://en.wikipedia.org/wiki/Sudan_function
   and implement it in OCaml; 
   use the tabulated values of sudan(n,x,y) there as tests for your code *)

(* (5 marks) Q2a: as a function of type int -> int -> int -> int

   use recursion directly, testing arguments using 'if then else' *)

(* The following code is my python implimentation of the sudan function
   converted to OCaml code by simply changing the syntax of the parameters and
   the if expression. All inputs are specified to be integers. *)

let rec sudan_int (n : int) (x : int) (y : int) = 
  if n = 0 then x + y
  else if y = 0 then x
  else sudan_int (n - 1) (sudan_int n x (y - 1)) ((sudan_int n x (y - 1)) + y);; 

let test_q2a_1 = (sudan_int 0 0 0 = 0);;
let test_q2a_2 = (sudan_int 2 2 1 = 27);;
let test_q2a_3 = (sudan_int 1 2 0 = 2);;
let test_q2a_4 = (sudan_int 2 3 1 = 74);;

(* (10 marks) Q2b: as a function of type nat -> nat -> nat -> nat 
   where the type nat is defined below; 
   you may use the supplied definition of addition on nats
   in your solution *)

type nat = Zero | Succ of nat 
             
let add_nat m n = 
  let rec helper m = match m with Zero -> n | Succ m -> Succ (helper m)
  in helper m

let zero_nat = Zero
let one_nat  = Succ zero_nat             
let two_nat  = Succ one_nat             
let four_nat  = add_nat two_nat two_nat

let test_q2b_1 = ((add_nat one_nat one_nat) = two_nat);;
  
(* The following code uses a match statement to decrement a number. *)

let minus_one_nat n = match n with
  | Zero -> Zero 
  | Succ nn -> nn;;
    
(* The following code is my implimentation of the sudan function using natural
   numbers. The way I was able to convert the implimentation from the one above
   using integers was by using the type definition of nat to define numbers that
   equal Zero. I also used the add_nat expression to replace the (+) operator
   and the minus_one_nat expression that I wrote to decrement any natural number
   I needed to. *)
        
let rec sudan_nat (n : nat) (x : nat) (y : nat) = match n, y with
  | n, y when n = Zero -> add_nat x y
  | n, y when y = Zero -> x
  | _ -> sudan_nat (minus_one_nat n) (sudan_nat n x (minus_one_nat y)) (add_nat (sudan_nat n x (minus_one_nat y)) y);;

let test_q2b_2 = ((sudan_nat one_nat two_nat two_nat) = (add_nat (add_nat four_nat four_nat) four_nat));;
  
(* ****************************** End of Question 2 ********************************* *)



(**** (10 marks) Q3: essay ****)

(* Write a short essay about the functional programming (FP) in OCaml, 
   illustrated with code examples as appropriate (* Which should compile correctly *)
   which addresses the following points:

   a. the roles of types in programs, including parametric polymorphism
   b. expressions evaluation, focusing on function application and pattern-matching
   c. the use of the Y combinator in implementing recursion
   d. contemporary applications of FP technology *)

(* a. 
     According to IBM, 'a type is a description of a set of values and a set of allowed
  operations on those values'. OCaml allows for expressions to do just this; they could 
  take parameters such as an integer, and a string, and return a third value like a 
  list.
     However, OCaml also allows parametic polymorphism, which, in laymans' terms is an
  expression which takes any type picked by the caller and performs the same function on it
  (Yorgey, 2013). Parametic polymorphism is an idea specified by the field of generic
  programming which presents the idea that algorithms can be written in terms where the types
  of variables can be specified later (Gibbons, 2003).
     A popular example of parametic polymorphism in OCaml are the List.fold_left and List.fold_right
  expressions because they can take parameters of many different types, such as a list of booleans,
  or a list of integers, etc... and applies a function to them (which is specified as another parameter); 
  and, regardless of their type, the List.fold expressions work.
     The difference between the parametic polymorphism of OCaml, and, say, the inheritance 
  polymorphism of other high level languages is that although both perform polymorphism, 
  inheritance polymorphism performs a different function with the same identity depending on
  the class in the hierarchy it is being used in, whereas parametic polymorphism applies 
  a different function with the same identity depending on the nature of its parameters.
     An example of List.fold_left applying 2 different functions depending on the type
  of data is below. *)

(* This example finds the disjunction of a list of booleans. *)

let fold_left_example_bool (bl : bool list) = List.fold_left (fun x y  ->  x || y) false bl;; 

let test_q3a_1 = (fold_left_example_bool [] = false);;
let test_q3a_2 = (fold_left_example_bool [false] = false);;
let test_q3a_3 = (fold_left_example_bool [true] = true);;
let test_q3a_4 = (fold_left_example_bool [true;false] = true);;
let test_q3a_5 = (fold_left_example_bool [false;true;true] = true);;
let test_q3a_6 = (fold_left_example_bool [false;false;false] = false);;

(* This example finds the sum of a list of integers. *)

let fold_left_example_int (il : int list) = List.fold_left (fun x y  ->  x + y) 0 il;;

let test_q3a_7 = (fold_left_example_int [] = 0);;
let test_q3a_8 = (fold_left_example_int [15] = 15);;
let test_q3a_9 = (fold_left_example_int [23] = 23);;
let test_q3a_10 = (fold_left_example_int [8;7] = 15);;
let test_q3a_11 = (fold_left_example_int [87;13] = 100);;
let test_q3a_12 = (fold_left_example_int [100;200;13] = 313);;

(* As can be seen, both use the same function but have wildly different natures.  *) 

(* b.
    Simply put, when an expression is evaluated, it returns a value. These expressions all take 
  parameters which are variables bound to (variable, value) pairs in an environment 
  (Srinivasan, 2021). 
    Function application is when a function is used as one or of these variables that an expression
  takes as an argument. In fact, it is a key ability of functional programming for an expression to
  take a function as an argument and to return one to it's caller as well (Sturtz, 2020). Thus, in
  OCaml, one can evaluate using expression with function application.
    Pattern matching is when an expression performs a different function on one or more parameters
  depending on what their values are. The expression, therefore, returns a different value or 
  function to its caller depending on the nature of its parameters. Thus, one can evaluate 
  data using an expression with mattern matching in OCaml. 
    Expression evaluation is quite similar in different languages such as Python and Java because
  both can pattern match and take functions as parameters, however, as not being functional
  programming languages themselves, they do not need to have these features, although they are
  certainly a plus to have.
    An example of all 3 being used simotaneously is below.*) 

(* func is an example of function application in an expression. exp_eval_example is an example of 
   pattern matching being used to evaluate an expression. *)

let exp_eval_example ls func = match ls with
  | [] -> -100
  | h ::t -> List.fold_left func 0 ls;;

let test_q3b_1 = (fold_left_example_int [] = -100);;
let test_q3b_2 = (fold_left_example_int [15] = 15);;
let test_q3b_3 = (fold_left_example_int [23] = 23);;
let test_q3b_4 = (fold_left_example_int [8;7] = 15);;
let test_q3b_5 = (fold_left_example_int [87;13] = 100);;
let test_q3b_6 = (fold_left_example_int [100;200;13] = 313);;

(* c.
    'The Y Combinator is a fixed-point higher-order function used to implement recursion in any 
  programming language that does not support it natively' (Riva, 2019). Because of this, it is a
  generalises recursion and abstracts its implimentation, meaning it does not need to be called from
  inside itself. Below is an implimentation of the Y combinator in OCaml. As can be seen, it does not
  use the rec keyword once. *)

type 'a mu = Roll of ('a mu -> 'a);;

let unroll (Roll x) = x;;

let fix f = (fun x a -> f (unroll x x) a) (Roll (fun x a -> f (unroll x x) a));;

(* d.
    The primary uses of functional programming technology are to programme symbolic computation
  and list processing applications. It should be noted that functions exist in all high level
  languages but only pure functional programming languages use the functional model (Indeed, 2022).
  Due to the fact that functional programming lanugages use conditional expressions and recursions
  to carry out their functions, they are very useful for implimenting mathematical functions. Thus,
  they are incredibly useful for programming in both the fields of Mathematics and Logic.
    Due to their use of immutable variables, FP languages are incredibly efficient and nearly
  entirely bug free in their applications. This makes them fantastic for the aforementioned fields
  as well as being useful in even general programming applications if one is looking for a language
  with the aforementioned qualities. *)

(*Gibbons, J. (2003) Datatype-Generic Programming, University of Oxford Department of Computer Science.
  University of Oxford. Available at: https://www.cs.ox.ac.uk/jeremy.gibbons/publications/dgp.pdf 
  (Accessed: November 22, 2022). 
  
  IBM, D. (2021) Type definitions. IBM. Available at: 
  https://www.ibm.com/docs/en/epfz/5.3?topic=reference-type-definitions (Accessed: November 21, 2022).

  Indeed (2022). What are functional programming languages and why use them? [online] uk.indeed.com. 
  Available at: https://uk.indeed.com/career-advice/career-development/functional-programming-languages
  [Accessed 1 Dec. 2022].

  Riva, M. (2019). Implementing Recursion with the Y Combinator in any Language. [online] Medium. Available 
  at: https://levelup.gitconnected.com/implementing-recursion-with-the-y-combinator-in-any-language-9e83fa369ca 
  [Accessed 30 Nov. 2022].

  Srinivasan, S. Teixeira, R. Qian, M. (2021) CSE 130: Programming Languages: Functional Programming, UC 
  San Diego. Available at: https://cseweb.ucsd.edu/classes/wi00/cse130/funcP (Accessed: November 29, 2022).

  Sturtz, J. (2020). Functional Programming in Python: When and How to Use It – Real Python. [online] 
  realpython.com. Available at: https://realpython.com/python-functional-programming 
  [Accessed 29 Nov. 2022].
  
  Yorgey, B. (2013) More polymorphism and type classes, 05-type-classes. University of Pennsylvania. 
  Available at: https://www.seas.upenn.edu/~cis1940/spring13/lectures/05-type-classes.html (Accessed: 
  November 22, 2022). *)

(* ****************************** End of Question 3 ********************************* *)


(**** (10 marks) Q4: the s, k and i combinators ****)

(* using the definitions below:

   a. write down the types of s, k and i
   b. what are the values of (i 3), (i true)?
   c. explain your answer to b. by showing in general how i evaluates its arguments *)

let k x y = x;;

let s x y z = (x z) (y z);;

let i = s k k;;

(*a. s, k and i are all have the type of function. This is because they are all expressions
	that take parameters and output a result based on those parameters.
	
	b. The value of (i 3) is 3 and the value of (i true) is an error.
	
	c. 
	The reason 3 was returned when (i 3) is run is because i takes a '_weak1 as a parameter and outputs 
  '_weak1 as well. This type is not actually an explicit type itself, it's an unknown type known as a 
  weak type variable (Ocamlverse, 2022). The compiler is saying that it is a mutable variable, but it 
  cannot infer the type yet because it hasn't collected enough information because i's output is dependant
  on other functions that may or may not output an integer (as far as the compiler is concerned). Thus, 
  if given an integer such as 3, the compiler will know that it's meant to output an integer as well, as
  it outputs the same type as it takes.
	
	The reason why an error is returned when (i true) is run is because although the compiler doesn't know what 
  the type is of what i takes before the code is run, due to the expressions that i calls (s and k) not 
  being able to work on booleans, it, therefore, isn't able to work on booleans either.

	OCamlverse. (n.d.). Weak Type Variables. [online] Available at: 
  https://ocamlverse.net/content/weak_type_variables.html [Accessed 18 Nov. 2022]. *)

let test_q4_1 = ((i 1) = 1);;           
let test_q4_2 = ((k 2 3) = 2);;
let test_q4_3 = ((s (fun x y -> y + x) (k 4) 5) = 9);;

(* ****************************** End of Question 4 ********************************* *)


(**** (15 marks total) Q5: triangular numbers ****)

(* (5 marks) Q5a. write a function ints : int -> int list which given an argument (n : int), 
   returns the list consisting of [0; 1; 2;...; (n-1)], provided n > 0, and returns [] if n <= 0. *)

(* The following code recursively adds the current index to a list until the index equals
   the goal number or the goal number is less than or equal to zero. *)

let ints (n : int) = 
  let i = 0 in
  let rec oper i n = match i, n with
    | i, n when n <= 0 -> [] (* Returns an empty list if the goal number is less than or equal to 0. *)
    | i, n when i >= n -> [] (* Returns an empty list if the index is greater than or equal to the goal number.
                                This ends the list if it is matched, and oper will not be called again. *)
    | _ -> i :: (oper (i+1) n) (* The recursive code that add the current index to the list itself. *)
  in oper i n;;

let test_q5a_1 = (ints 0 = []);;    
let test_q5a_2 = (ints 3 = [0;1;2]);;
let test_q5a_3 = (ints 4 = [0;1;2;3]);;
let test_q5a_4 = (ints 5 = [0;1;2;3;4]);;
let test_q5a_5 = (ints (-1) = []);;

(* (5 marks) Q5b. implement the function tri : int -> int which given an argument (n : int), 
      returns (n - 1) + (n - 2) + ... + 1 + 0, using the function ints defined in a. *)

(* The following code recursively adds each element in the list fed into tri using the ints
   expression into a final sum until there are no elements left in the list. *)

let tri (n : int) = (* tri is given an integer that specifies the length of the list to be created. *)
  let list = ints n (* ints creates a list of length n that increments from 0 to n-1 each element. *) in 
  let rec oper list = match list with
    | [] -> 0 (* This is the base case that is added to the final list or returned if list is empty. *)
    | h::t  ->  h + (oper t) (* This is the recursive case that adds all elements of the list recursively. *)
  in oper list;;

let test_q5b_1 = (tri 0 = 0);; 
let test_q5b_2 = (tri 1 = 0);;
let test_q5b_3 = (tri 2 = 1);;
let test_q5b_4 = (tri 3 = 3);;
let test_q5b_5 = (tri 4 = 6);;
let test_q5b_6 = (tri 5 = 10);;
let test_q5b_7 = (tri (-1) = 0);;


(* (5 marks) Q5c. how else might you implement the function tri directly?
      how would you try to show that the two implementations agree? *)

(* I would impliment it directly by adding a recursive expression that adds whatever the
	current value of n is to itself - 1 until n = 0. If the base case is hit, it will return
	0, and finish the recursion so that (n-1) + (n-2) + (n-3) + ... + 1 + 0 is added. To make
	sure that this would work, I would manually set n to be n minus 1 so that n is not added to
	that total. A code example of how I would do this is below. *)

let tri_alt (n : int) =
  let n = n - 1 in (* Immediately decrement n. Specifies n-1 being the largest number to be added. *)
  let rec oper n = match n with
    | n when n <= 0 -> 0 (* This is the base case that stops adding n recursively when it's 0. *)
    | _ -> n + oper (n-1) (* Recursively adds n and then decrements it. *)
  in oper n;;

(* As for how I'd make sure the two implimentations agree, I'd give both the same data
	(multiple values to make sure it's not a fluke) and assert whether both would output the same
	sum. *)

let test_q5c_1 = (tri 0 = tri_alt 0);; 
let test_q5c_2 = (tri 1 = tri_alt 1);;
let test_q5c_3 = (tri 2 = tri_alt 2);;
let test_q5c_4 = (tri 3 = tri_alt 3);;
let test_q5c_5 = (tri 4 = tri_alt 4);;
let test_q5c_6 = (tri 5 = tri_alt 5);;
let test_q5c_7 = (tri (-1) = tri_alt (-1));;

(* ****************************** End of Question 5 ********************************* *)

(**** 20 marks total) Q6: expression trees ***)

(* (5 marks) Q6a: show how to express the following type of expression trees in OCaml:

   an expression tree consists of either: 
   - a literal integer constant, eg (Con 3)
   - a 'variable', identified by a string value, eg (Var "v42")
   - two subtrees connected with the binary operator Plus

*)

(* The following code defines tree as having 3 possible types. *)

type tree = 
  | Con of int
  | Var of string
  | Plus of tree * tree;;

(* (5 marks) Q6b: write down the corresponding 'fold' operator for the type exp_tree *)

(* The following code defines tree as having 3 possible types to be folded. The first 
   one is a constant that returns an integer when matched. The second one is a string that
   returns the integer 0 when matched (but will never be called due to the implimentation of
   tree_eval). The final type is a type that has 2 seperate sub-trees that are added together
   using the (+) operator. *) 

let rec tree_fold tree = match tree with
  | Con x -> x 
  | Var y -> 0 (* Due to the implimentation of tree_eval, this will never be called. *)
  | Plus (tree1, tree2) -> (+) (tree_fold tree1) (tree_fold tree2);; 

let test_q6b_1 = (tree_fold (Plus ((Con 3), (Con 4))) = 7);;
let test_q6b_2 = (tree_fold (Plus ((Con 5), (Con 8))) = 13);;

(* (10 marks) Q6c: show how to express an evaluation function for trees, 
        taking an argument env of type (string * int) list, 
        and returning an int, such that:

   - literal constants evaluate to themselves
   - Plus expressions evaluate to the sum of the values of their components

   In your answer, pay careful attention to what kinds of potential errors might arise, 
   and how you might handle them
*) 

(* Folds a tree if it is of type Con, if it is of type Var, it matches Var to an list
   of tuples (string, int), where if Var matches the string in the tuple, the integer
   in said tuple will be returned. If it is of type Plus, tree_eval is called recursively
   on each subtree of Plus, and the results of this are folded. *) 

let rec tree_eval env tree =
  match tree with
  | Con x -> tree_fold tree
  | Var y -> tree_fold (Con (let rec find_int y env = 
                               match env with
                               | [] -> raise Not_found (* Does this if there is no environment inside the env list. *)
                               | (string, integer)::tail -> 
                                   match string with
                                   | string when string = y -> integer (* Returns integer if the string is equal to Var. *)
                                   | _ -> find_int y tail (* Looks at the next tuple in the env list. *)
                             in find_int y env))
  | Plus (tree1, tree2) -> tree_fold (Plus ((Con (tree_eval env tree1)), (Con (tree_eval env tree2))));; 


let test_q6c_1 = (tree_eval [] (Con 3) = 3);;         
let test_q6c_2 = (tree_eval [] (Plus ((Con 3), (Con 4))) = 7);;
let test_q6c_3 = (tree_eval [("v1",4);("v2",3)] (Plus ((Var "v1"), (Var "v2"))) = 7);;


(* ****************************** End of Question 6 ********************************* *)

(**** (15 marks) BONUS Q7: program comprehension/list comprehensions ****)                

(* This question asks you to consider the following function, `scp`, defined in Python *) 

(* 
* *** start of definition of function `scp` *** *

def scp(xss):
    if [] == xss: return [[]]  
    else: return [ [x] + xs for x in xss[0] for xs in scp(xss[1:]) ]

* *** end of definition of function `scp *** *
                                    *)

(* (5 marks) Q7a. by considering the possible types of the function `scp`, 
   and its behaviour on suitable values of the correct type(s), 
   describe what `scp` does *)

(* What scp does is it takes a list of length n and applies all the elements
   in xss[0] to each of the other elements in xss. For example if xss[0] was a
   list of size 2, there would be len(xss) - 1 number of elements with xss[0][0]
   preceeding all elements in the sublists, and len(xss) - 1 number of elements with
   xss[0][1] preceeding all elements in the sublists, totalling in 2(len(xss)) - 2
   number of elements in the xss that is returned.
     
   This is unless one of two cases are met. 
    - If xss has only elements of Length 1, then xss will be flattened. 
    - If xss has one element, xss[0] will be expanded so that xss will be the same 
      size of xss[0]. Each element in xss[0] will become a single element of a 
      sublist that is an element of xss in the order that it existed in xss[0]

*)

(* (10 marks) Q7b. write an equivalent definition of the function `scp`, in OCaml 

                     *** as an OCaml function ***.

   NB you may NOT use OCaml extensions that support list comprehension syntax directly.  
   Answers that do this may score zero marks.
*) 

(* I know this pattern matching isn't exhaustive but it covers 3 of the 5 cases and I've
   put 15 hours into this so here it is. *)
let scp xss = match xss with
  | [[]] -> [] 
  | [] -> [[]]
  | _ -> [let origin = xss in (* The following code flattens the list. *)
          let rec flatten accumulator flatlist origin =
            match flatlist with
            | x :: tail -> flatten (x :: accumulator) tail origin
            | [] ->
                match origin with
                | [] -> List.rev accumulator
                | x :: tail -> flatten accumulator x tail
          in flatten [] [] origin] ;; 

let test_q7_1 = (scp [] = [[]]);;        
let test_q7_2 = (scp [[]] = []);;
let test_q7_3 = (scp [[1];[2];[3];[4]] = [[1;2;3;4]]);;
let test_q7_4 = (scp [[1]] = [[1]]);;

(* ****************************** End of Questions ********************************* *) 