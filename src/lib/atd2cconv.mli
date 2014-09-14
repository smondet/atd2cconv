
val transform_module_item: Atd_ast.module_item ->
  [ `Ok of SmartPrint.t
  | `Error of [> `Not_implemented of string ]]
