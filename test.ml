module MyStack : STACK = struct
  type 'a t = 'a list
  
  let empty = []
  
  let is_empty stack = stack = []
  
  let push x stack = x :: stack
  
  let peek stack =
    match stack with
    | [] -> None
    | x :: _ -> Some x
  
  let pop stack =
    match stack with
    | [] -> None
    | x :: xs -> Some xs
  
  let size stack = List.length stack
end
module StackTester (Stack : STACK) = struct
  exception BuildFailure
  
  type 'a build =
    | Empty
    | Push of 'a * 'a build
    | Pop of 'a build
  
  type 'a check =
    | IsEmpty of bool
    | Peek of ('a -> bool) option
    | Pop of unit option
    | Size of int
  
  let rec build build_spec =
    match build_spec with
    | Empty -> Stack.empty
    | Push (x, b) -> Stack.push x (build b)
    | Pop b -> match Stack.pop (build b) with
      | None -> raise BuildFailure
      | Some s -> s
  
  let test stack check_spec =
    match check_spec with
    | IsEmpty b -> Stack.is_empty stack = b
    | Peek None -> Stack.peek stack = None
    | Peek (Some f) -> (match Stack.peek stack with
                        | None -> false
                        | Some x -> f x)
    | Pop None -> (match Stack.pop stack with
                   | None -> false
                   | Some _ -> true)
    | Pop (Some ()) -> (match Stack.pop stack with
                        | None -> false
                        | Some stack' -> Stack.is_empty stack')
    | Size n -> Stack.size stack = n
    let stack = MyStack.create ()

  
  let print f stack =
    List.iter (fun x -> print_string (f x); print_string " ") stack;
    print_newline ()
end
module StackTester (Stack : STACK) = struct
  exception BuildFailure
  
  type 'a build =
    | Empty
    | Push of 'a * 'a build
    | Pop of 'a build
  
  type 'a check =
    | IsEmpty of bool
    | Peek of ('a -> bool) option
    | Pop of unit option
    | Size of int
  
  let rec build build_spec =
    match build_spec with
    | Empty -> Stack.empty
    | Push (x, b) -> Stack.push x (build b)
    | Pop b -> match Stack.pop (build b) with
      | None -> raise BuildFailure
      | Some s -> s
  
  let test stack check_spec =
    match check_spec with
    | IsEmpty b -> Stack.is_empty stack = b
    | Peek None -> Stack.peek stack = None
    | Peek (Some f) -> (match Stack.peek stack with
                        | None -> false
                        | Some x -> f x)
    | Pop None -> (match Stack.pop stack with
                   | None -> false
                   | Some _ -> true)
    | Pop (Some ()) -> (match Stack.pop stack with
                        | None -> false
                        | Some stack' -> Stack.is_empty stack')
    | Size n -> Stack.size stack = n
  
  let print f stack =
    List.iter (fun x -> print_string (f x); print_string " ") stack;
    print_newline ()
end
