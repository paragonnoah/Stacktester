module type STACK = sig
  type t
  
  (** Stack is empty *)
  val empty : t
  
  (** Stack empty? *)
  val is_empty : t -> bool
  
  (** Push an element to the top of the stack and return new stack *)
  val push : int -> t -> t
  
  (** Peek the top element of the stack *)
  val peek : t -> int option
  
  (** Remove from the stack *)
  val pop : t -> t option

  (** Size of the stack*)
  val size : t -> int
end

module ListStack : STACK = struct
  type t = int list
  
  let empty = []

  let is_empty stack =
    match stack with
    | [] -> true
    | _ -> false

  let push id stack =
    id :: stack

  let peek stack =
    match stack with
    | [] -> None
    | x :: _ -> Some x

  let pop stack =
    match stack with
    | [] -> None
    | _ :: xs -> Some xs

  
  let size stack =
    List.length stack
end
