
val transform_module_item: Atd_ast.module_item ->
  [ `Ok of SmartPrint.t
  | `Error of [> `Not_implemented of string ]]
(** Transform a single ATD type definition to an OCaml module: {[
      module Type_name = struct
        type (…, …) t = …
        let source = CConv.Source.(…)
        let sink = CConv.Sink.(…)
    ]}*)
