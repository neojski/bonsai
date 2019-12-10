open! Core_kernel

type t =
  | Packed : 'a * 'a Type_equal.Id.t -> t
  | External_event : string -> t
  | No_op : t
  | Sequence : t list -> t

let pack type_id t = Packed (t, type_id)
let sequence events = Sequence events
let no_op = No_op