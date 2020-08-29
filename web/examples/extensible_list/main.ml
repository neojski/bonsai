open! Core_kernel
open! Bonsai_web
open! Bonsai.Infix

module Row = struct
  type t =
    { id: int
    ; value : string
    }
  [@@deriving equal, sexp]

  let test = { id = 0; value = "test" }

  let _x = test
end

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

let _row2 =
  Bonsai.pure ~f:(fun { Row.id; value } ->
      Vdom.Node.div
        []
        [ Vdom.Node.textf "(pure) key: %d; value: %s" id value ])

let row3 =
  Bonsai.With_incr.pure ~f:(fun incr ->
      let%map.Incr { Row.id; value } = incr in
      Vdom.Node.div
        []
        [ Vdom.Node.textf "(pure) key: %d" id
        ; Vdom.Node.textf "(pure) value: %s" value
        ])

let map component =
  let associated = Bonsai.Map.assoc_input (module Int) component in
  let%map.Bonsai y = associated in
  let z =  Map.data y in
  Vdom.Node.div [] z

let (_ : _ Start.Handle.t) =
  let incr =
    let init = 
      List.init 10000 ~f:(fun id ->
          id, { Row.id; value = Int.to_string id })
      |> Int.Map.of_alist_exn
    in
    let var = Incr.Var.create init in

    (
      let i = ref 0 in
      let open Async_kernel in
      Clock_ns.every (Time_ns.Span.of_int_ms 10) (fun () ->
          incr i;
          let key = !i in
          Incr.Var.replace var ~f:(fun map ->
              Map.update map key ~f:(fun row ->
                  match row with
                  | Some { Row.id; value } -> { Row.id; value = value ^ "x" }
                  | None -> { Row.id = key; value = "new" }
            ))
      )
    );

    Incr.Var.watch var
  in
  let component = Bonsai.With_incr.of_incr incr >>> map row3 in
  Start.start_standalone ~initial_input:incr ~bind_to_element_with_id:"app" component
;;
