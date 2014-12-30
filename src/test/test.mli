
(** The type to be tested. *)
type t

(** Test that we can convert this type to JSON and back. *)
val test : t -> unit

(** Run test over a list of elements. *)
val test_list : t list -> unit

