module type STACK = sig
  type 'a t
  
  (** Stack is empty *)
  val empty : 'a t
  
  (** Stack empty? *)
  val is_empty : 'a t -> bool
  
  (** push an element to the top of the stack and returns new stack *)
  val push : 'a -> 'a t -> 'a t
  
  (** peek the top element of the stack *)
  val peek : 'a t -> 'a option
  
  (** removes from the stack *)
  val pop : 'a t -> 'a t option

  (** size of the stack*)
  val size : 'a t -> int
end

(** Recipe for Building the stack *)
type 'a build =
  | Empty
  | Push of 'a * 'a build
  | Pop of 'a build

(** Stack Validation *)
type 'a check =
  | IsEmpty of bool
  | Peek of ('a -> bool) option
  | Pop of unit option
  | Size of int

(** Module to test the implementation *)
module StackTester (Stack : STACK) :
sig
  exception BuildFailure
  
  (* Build the stack using the recipe *)
  val build : 'a build -> 'a Stack.t
  
  (** [test s c] is [true] if and only if [s] satisfies the conditions specified by [c]. *)
  val test : 'a Stack.t -> 'a check -> bool
  
  (** Prints the elements of the stack *)
  val print : ('a -> string) -> 'a Stack.t -> unit
end
=
struct
  exception BuildFailure

  let rec build_helper recipe acc =
    match recipe with
    | Empty -> acc
    | Push (x, rest) -> build_helper rest (Stack.push x acc)
    | Pop rest -> match Stack.pop acc with
                  | Some s -> build_helper rest s
                  | None -> raise BuildFailure
open stack
  let build recipe = build_helper recipe Stack.empty

  let is_empty (stack : int Stack.t) : bool =
    match Stack.top_opt stack with
    | None -> true
    | Some (Pop _) -> (match Stack.pop stack with Some (Pop _) -> false | Some Empty -> true | _ -> false)
    | Some Empty -> false
  
  let test stack = function
    | IsEmpty b -> Stack.is_empty stack = b
    | Peek None -> Stack.peek stack = None
    | Peek (Some p) -> match Stack.peek stack with
                       | Some x -> p x
                       | None -> false
                       | Pop -> (match Stack.pop stack with Pop _ -> false | Empty -> true)
                       | Pop (Some ()) -> (match Stack.pop stack with Some s -> Stack.is_empty s | None -> false)
    | Size n -> Stack.size stack = n
  

  let print f stack =
    let rec print_helper = function
      | None -> ()
      | Some s -> (print_helper (Stack.pop s);
                   match Stack.peek s with
                   | None -> ()
                   | Some x -> print_string (f x ^ "\n"))
    in
    print_helper (Some stack)
end

module MyStack : STACK = struct
  type 'a t = 'a list

  let empty = []

  let is_empty = function
    | [] -> true
    | _ -> false

  let push x s = x :: s

  let peek = function
    | [] -> None
    | x :: _ -> Some x

  let pop = function
    | [] -> None
    | _ :: xs -> Some xs

  let size s = List.length s
end

module MyStackTester = StackTester(MyStack)

(* Run the tests *)
let test1 = Push (1, Push (2, Push (3, Pop (Pop Empty))))
let result1 = MyStackTester.build test1
let test1_passed = MyStackTester.test result1 (Peek (Some 1)) && MyStackTester.test result1 (Size 2)

let test2 = Push (10, Push (9, Push (5, Push (1, Push (2, Pop (Pop (Push (8, Push (6, Empty)))))))))
let result2 = MyStackTester.build test2
let test2_passed = MyStackTester.test result2 (Peek (Some 5)) && MyStackTester.test result2 (Size 3)

let test3 = Push (100, Push (200, Push (300, Pop (Pop (Push (400, Push (500, Empty)))))))
let result3 = MyStackTester.build test3
let test3_passed = MyStackTester.test result3 (Peek (Some 400)) && MyStackTester.test result3 (Size 3)

let test4 = Push (8, Push (5, Push (3, Pop (Push (4, Push (1, Empty))))))
let result4 = MyStackTester.build test4
let test4_passed = MyStackTester.test result4 (Peek (Some 4)) && MyStackTester.test result4 (Size 4)

let test5 = Push (1000, Push (2000, Push (3000, Pop (Push (10000, Pop (Push (5000, Empty)))))))
let result5 = MyStackTester.build test5
let test5_passed = MyStackTester.test result5 (Peek (Some 10000)) && MyStackTester.test result5 (Size 3)

(* Print the results *)
let print_result test_number result =
Printf.printf "Test %d: %s\n" test_number (if result then "Passed" else "Failed")

let () =
print_result 1 test1_passed;
print_result 2 test2_passed;
print_result 3 test3_passed;
print_result 4 test4_passed;
print_result 5 test5_passed;
