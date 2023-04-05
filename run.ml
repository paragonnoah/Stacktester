module type STACK = sig
  type 'a t
  
  (** [empty] returns an empty stack *)
  val empty : 'a t
  
  (** [is_empty s] returns [true] if the stack [s] is empty, and [false] otherwise *)
  val is_empty : 'a t -> bool
  
  (** [push x s] adds element [x] to the top of the stack [s], and returns the resulting stack *)
  val push : 'a -> 'a t -> 'a t
  
  (** [peek s] returns the top element of stack [s] *)
  val peek : 'a t -> 'a option
  
  (** [pop s] removes and returns the top element of stack [s], and returns [None] if the stack is empty *)
  val pop : 'a t -> 'a t option

  (** [size s] returns the number of elements in the stack [s] *)
  val size : 'a t -> int
end

(* Recipe for building the stack *)
type 'a build =
  | Empty
  | Push of 'a * 'a build
  | Pop of 'a build

(* Stack validation *)
type 'a check =
  | IsEmpty of bool
  | Peek of ('a -> bool) option
  | Pop of unit option
  | Size of int

(* Module to test the implementation *)
module StackTester (Stack : STACK) = struct
  exception BuildFailure
  
  (* Build the stack using the recipe *)
  let build recipe =
    let rec build' s = function
      | Empty -> s
      | Push (x, r) -> build' (Stack.push x s) r
      | Pop r ->
        (match Stack.pop s with
          | None -> raise BuildFailure
          | Some s' -> build' s' r)
    in build' Stack.empty recipe
  
  (* Test a stack against a check *)
  let test stack = function
    | IsEmpty b -> Stack.is_empty stack = b
    | Peek p -> (match Stack.peek stack with
                  | None -> p = None
                  | Some x -> match p with
                              | None -> false
                              | Some p' -> p' x)
    | Pop b -> (match Stack.pop stack with
                | None -> b = None
                | Some s -> match b with
                            | None -> false
                            | Some () -> true)
    | Size n -> Stack.size stack = n
  
  (* Print the elements of the stack *)
  let print f stack =
    let rec print' = function
      | None -> ()
      | Some s -> (print' (Stack.pop s); match Stack.peek s with
                                          | None -> ()
                                          | Some x -> print_string (" " ^ f x))
    in (print' (Some stack); print_newline ())
end

(* Implementation of a LIFO stack *)
module MyStack : STACK = struct
  type 'a node = {
    value : 'a;
    mutable next : 'a node option;
  }

  type 'a t = 'a node option ref

  let create () = ref None

  let push stack v =
    let n = {value = v; next = !stack} in
    stack := Some n

  let pop stack =
    match !stack with
    | None -> None
    | Some n ->
      stack := n.next;
      Some n.value

  let size stack =
    let rec count acc = function
      | None -> acc
      | Some n -> count (acc+1) n.next
    in count 0 !stack

  let peek stack =
    match !stack with
    | None -> None
    | Some n -> Some n.value

  let is_empty stack =
    match !stack with
    | None -> true
    | Some _ -> false

  let empty = create ()
end (* end of MyStack *)
   
open Stack

let main () =
  let module Tester = StackTester (MyStack) in
  
  let test1 = Push (1, Push (2, Push (3, Pop (Pop Empty)))) in
  let result1 = Tester.build test1 in
  let test1_passed = Tester.test result1 (Peek (Some 1)) in
  
  let test2 = Push (5, Push (2, Push (3, Pop (Pop Empty)))) in
  let result2 = Tester.build test2 in
  let test2_passed = Tester.test result2 (Peek (fun x -> x = Some 5)) in
  
  let test3 = Push (10, Push (9, Push (8, Push (7, Push (6, Push (5, Push (4, Push (3, Push (2, Push (1, Pop (Pop (Push (8, Pop (Pop (Push (3, Empty)))))))))))))))) in
  let result3 = Tester.build test3 in
  let test3_passed = Tester.test result3 (Size 4) in
  
  let test4 = Push (9, Push (8, Push (7, Push (6, Push (5, Push (4, Push (3, Push (2, Push (1, Pop (Pop (Pop Empty))))))))))) in
  let result4 = Tester.build test4 in
  let test4_passed = Tester.test result4 (IsEmpty true) in
  
  let test5 = Push (9, Push (8, Push (7, Push (6, Push (5, Push (4, Push (3, Push (2, Push (1, Empty))))))))) in
  let result5 = Tester.build test5 in
  let test5_passed = Tester.test result5 (Size 9) in
  
  let all_tests_passed = test1_passed && test2_passed && test3_passed && test4_passed && test5_passed in
  
  if all_tests_passed then
    print_endline "All tests passed!"
  else
    print_endline "At least one test failed."
  
let () = main ()

      
    
