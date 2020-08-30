open! Core_kernel
open! Bonsai_web
open! Bonsai.Infix

module Row = struct
  type t =
    { id: int
    ; value : string
    }
  [@@deriving equal, sexp]

  let component =
    Bonsai.pure ~f:(fun { id; value } ->
        Vdom.Node.div
          [ Vdom.Attr.id (Int.to_string id) ]
          [ Vdom.Node.textf "(pure) key: %d" id
          ; Vdom.Node.textf "(pure) value: %s" value
          ])
end

(* the most elaborate 
   module Row_component = struct
   module Input = Row
   module Result = Vdom.Node
   module Model = Unit

   module Action = struct
    type t [@@deriving sexp]
   end

   let apply_action ~inject:_ ~schedule_event:_ _ _ = assert false

   let compute ~inject:_ { Row.id; value } () =
    Vdom.Node.div
      []
      [ Vdom.Node.textf "(elaborate) key: %d; value: %s" id value ]

   let name = Source_code_position.to_string [%here]
   end
   let _row = Bonsai.of_module ~default_model:() (module Row_component)
*)

let map component =
  let associated = Bonsai.Map.assoc_input (module Int) component in
  let%map.Bonsai list =
    Bonsai.With_incr.map associated ~f:(fun m ->
        (* CR: how do I make this actually incremental?

           I feel like I actually need some "Vdom.Node.Map" or something like that to make it efficient.
        *)
        let%map.Incr list =
          Incr_map.unordered_fold
            m
            ~init:[]
            ~add:(fun ~key:_ ~data acc -> data :: acc)
            ~remove:(fun ~key:_ ~data:_ acc -> (* what here? *) acc)
        in
        Vdom.Node.div [] list)
  and total =
    associated >>| fun x ->
    Vdom.Node.textf "total: %d" (Map.length x)
  in
  Vdom.Node.div [] [total; list]

module Big_map = struct
  type t = Row.t Int.Map.t [@@deriving equal, sexp]

  let default =
    List.init 1000 ~f:(fun id ->
        id, { Row.id; value = Int.to_string id })
    |> Int.Map.of_alist_exn
end

module Add_entry_component = struct
  module Input = Unit
  module Model = struct
    type t =
      { position: int
      ; big_map : Big_map.t
      } [@@deriving sexp, equal]
  end

  module Action = struct
    type t = Add_another_counter [@@deriving sexp]
  end

  module Result = struct
    type t = Big_map.t * Vdom.Node.t
  end

  let apply_action ~inject:_ ~schedule_event:_ () { Model.position; big_map } = function
    | Action.Add_another_counter ->
      let key = position in
      let big_map = Map.add_exn big_map ~key ~data:{ Row.id = key; value = Int.to_string key} in
      { Model.position = key - 1; big_map }
  ;;

  let compute ~inject () { Model.big_map; position = _ } =
    let on_click = Vdom.Attr.on_click (fun _ -> inject Action.Add_another_counter) in
    let view = Vdom.Node.button [ on_click ] [ Vdom.Node.text "Add Another Counter" ] in
    big_map, view
  ;;

  let name = Source_code_position.to_string [%here]
end

let add_entry_component =
  Bonsai.of_module (module Add_entry_component) ~default_model:{ position = -1; big_map = Big_map.default }
;;

let application =
  let open Bonsai.Let_syntax in
  let rows_component = map Row.component in
  let%map rows, add_button =
    add_entry_component >>> (Bonsai.Arrow.first rows_component)
  in
  Vdom.Node.div [] [ add_button; rows ]
;;

let (_ : _ Start.Handle.t) =
  Start.start_standalone ~initial_input:() ~bind_to_element_with_id:"app" application
;;
