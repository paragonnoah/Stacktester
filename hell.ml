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

let rec build (b : 'a build) : 'a MyStack.t =
  match b with
  | Empty -> MyStack.empty
  | Push (x, b') -> MyStack.push x (build b')
  | Pop b' -> (
      match MyStack.pop (build b') with
      | Some s -> s
      | None -> raise BuildFailure
    )

let test (s : 'a MyStack.t) (c : 'a check) : bool =
  match c with
  | IsEmpty b -> MyStack.is_empty s = b
  | Peek p -> (
      match MyStack.peek s with
      | Some x -> (
          match p with
          | Some f -> f x
          | None -> true
        )
      | None -> false
    )
  | Pop p -> (
      match MyStack.pop s with
      | Some s' -> (
          match p with
          | Some () -> true
          | None -> true
        )
      | None -> false
    )
  | Size n -> MyStack.size s = n
