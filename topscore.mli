(*
  Accumulator of the top N values over a stream of values.
  Each value is associated with a score and each value is considered
  unique.
*)

type ('a, 'b) t
  (* Accumulator of the top N values. *)

val create : ('a -> 'a -> int) -> int -> 'a -> 'b -> ('a, 'b) t
  (* 
     [create cmp n min_score dummy_data] returns an accumulator
     of the top [n] values of type ['b] based on their score of type ['a]
     and on the comparison function [cmp].
  *)
  
val add : ('a, 'b) t -> 'a -> 'b -> unit
  (* [add accu score data] *)

val get : ('a, 'b) t -> int * ('a * 'b) array
  (* Return the total count and the top N sorted (score, data) pairs. *)


(* Specialized version for score of type int. *)
module Int :
sig
  type 'a t
  val create : int -> int -> 'a -> 'a t
  val add : 'a t -> int -> 'a -> unit
  val get : 'a t -> int * (int * 'a) array
end

(* Specialized version for score of type float. *)
module Float :
sig
  type 'a t
  val create : int -> float -> 'a -> 'a t
  val add : 'a t -> float -> 'a -> unit
  val get : 'a t -> int * (float * 'a) array
end
