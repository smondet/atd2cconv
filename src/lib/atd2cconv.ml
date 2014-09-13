open Nonstd
open Printf

module Out = struct
  include SmartPrint
  let fmt fmt =
    Printf.ksprintf (string) fmt
  let (%) = (^-^)
  let comment c = string "(* " % c % string " *)"
  let commentf fmt =
    ksprintf (fun c -> string "(* " % string c % string " *)") fmt
  let cmtf fmt =
    ksprintf (fun c -> string "(* " % string c % string " *)" % newline) fmt
  let comma = string "," % space
end

let say fmt = Printf.(ksprintf (eprintf "%s\n%!") fmt)


let atom ?(t=Out.empty) ?(source=Out.empty) ?(sink=Out.empty) () =
  (`T t, `Source source, `Sink sink)

let (>>=) (`T t, `Source source, `Sink sink) f = 
  f ~t ~source ~sink

let (+++) a b =
  a >>= fun ~t ~source ~sink  ->
  b >>= fun ~t:t2 ~source:o2 ~sink:i2  ->
  let open Out in
  atom ~t:(t % t2) ~source:(source % o2) ~sink:(sink % i2) ()

let empty_atom = atom ()
let not_implemented msg =
  let open Out in
  atom ~t:(string "unit" % space % comment (fmt "not implemented: %s" msg)) ()
    ~source:(fmt "assert false")
    ~sink:(fmt "assert false")

let transform_expr ~self_name ?from expr =
  let open Out in
  let assert_false = fmt "assert false" in
  let rec go_deep = function
  | `Sum (loc, variants, ann) -> 
    let backtick n = fmt "`%s" n in
    let variant_case name expr =
      match expr with
      | Some e ->
        e >>= fun ~t ~source ~sink ->
        let t = fmt  "  | " % name % fmt " of " % t % newline in
        atom ~t ~source ~sink ()
      | None ->
        let t = fmt  "  | " % name % newline in
        atom ~t () in
    List.fold ~init:empty_atom variants ~f:(fun prev v ->
        prev >>= fun ~t:t1 ~source:o1 ~sink:i1  ->
        match v with
        | `Variant (loc, (name, ann), expr_opt) ->
          variant_case (backtick name) Option.(map expr_opt ~f:go_deep)
          >>= fun ~t ~source ~sink  ->
          atom ~t:(t1 % t) ~source:(o1 % source) ()
        | `Inherit (loc, expr) ->
            go_deep expr >>= fun ~t ~source ~sink ->
            let t = t1 % string " | " % t % newline in 
            atom ~t ~source:o1 ~sink:i1 ())
    >>= fun ~t ~source ~sink ->
    let t = brakets (newline % indent t) in
    atom ~t ~source:assert_false ~sink:assert_false ()
  | `Tvar (loc, var_name) ->
    let t = 
      (* comment (fmt "Tvar %S" var_name) % newline *)
      fmt "'%s" var_name in
    let source = fmt "%s_source" var_name in
    let sink =  fmt "%s_sink" var_name in
    atom ~t ~source ~sink ()
  | `Name (_, (loc, t_name, t_args), _) ->
    let with_mod_name v = 
      match t_name, from with
      | "int", _ -> string "int"
      | "float", _ -> string "float"
      | "string", _ -> string "string"
      | "list", _ -> string "list"
      | "abstract", Some f ->
        string (String.capitalize f) % string "." % string v
      | other, _ when other = self_name -> 
        string "t"
      | other, _ -> 
        string (String.capitalize other) % string "." % string v
    in
    List.fold t_args ~init:empty_atom ~f:(fun prev arg_expr ->
        prev >>= fun ~t:t1 ~source:o1 ~sink:i1  ->
        go_deep arg_expr >>= fun ~t:t2 ~source:o2 ~sink:i2  ->
        atom ()
          ~t:(t1 % comma % t2)
          ~source:(o1 % comma % o2)
          ~sink:(i1 % comma % i2))
    >>= fun ~t ~source ~sink  ->
    let t = 
      commentf "t_name: %s" t_name % space
      % (if t = empty then empty else (parens t % space))
      % with_mod_name "t" in
    let source = assert_false in
    let sink = assert_false in
    atom ~t ~source ~sink ()
  | `Tuple (loc, [], annot) -> assert false
  | `Tuple (loc, cell :: cell_list, annot) ->
    let (_, expr, _) = cell in
    let init = go_deep expr in
    List.fold ~init cell_list ~f:(fun prev (_, expr, _) ->
        prev >>= fun ~t:t1 ~source:o1 ~sink:i1  ->
        go_deep expr >>= fun ~t:t2 ~source:o2 ~sink:i2 ->
        atom ()
          ~t:(t1 % fmt " * " % t2)
          ~source:(o1 % comma % o2)
          ~sink:(i1 % comma % i2))
    >>= fun ~t ~source ~sink  ->
    atom () ~t:(parens t) ~source ~sink
  | `List (_, expr, _) ->
    go_deep expr
    >>= fun ~t ~source ~sink  ->
    atom ()
      ~t:(t % fmt " array")
      ~source:(source )
      ~sink:(sink )
  | `Option (_, expr, _) ->
    go_deep expr
    >>= fun ~t ~source ~sink  ->
    atom ()
      ~t:(t % fmt " option")
      ~source:(source )
      ~sink:(sink )
  | `Record (_, field_list, _) ->
    List.fold ~init:empty_atom field_list ~f:(fun prev f ->
        prev >>= fun ~t:t1 ~source:o1 ~sink:i1  ->
        match f with
        | `Field (_, (name, kind, _), expr) ->
          go_deep expr >>= fun ~t:t2 ~source:o2 ~sink:i2  ->
          atom ()
            ~t:(t1 % fmt "%s: " name % indent (t2) % fmt ";" % newline)
            ~source:(o1 % o2 % fmt ";" % newline)
            ~sink:(i1 % i2 % fmt ";" % newline)
        | `Inherit (_, expr) ->
          failwith "Not implemented: Inherit record fields")
    >>= fun ~t ~source ~sink  ->
    atom ()
      ~t:(braces (newline % t |> indent))
      ~source:(source % fmt "()") ~sink:(sink % fmt "()")
  | `Nullable (_, expr, _) ->
    not_implemented "Nullable"
  | `Shared (_, expr, _) ->
    not_implemented "Shared"
  | `Wrap (_, expr, _) ->
    not_implemented "Wrap"
  in
  go_deep expr

let get_from_annotation ann =
  List.find_map ann ~f:(fun (section, (loc, fields)) ->
      match section with
      | "ocaml" ->
        List.find_map fields ~f:(function
          | "from", (loc, val_opt) -> val_opt
          | other, _ -> None)
      | other -> say "NOC" ; None)

let transform_moditem =
  let open Out in
  function
  | `Type (loc, (name, param, ann), expr) ->
    let from = get_from_annotation ann in
    let self_name = name in
    transform_expr ?from ~self_name expr >>= fun ~t ~source ~sink ->
    let t =
      comment (fmt "Type %s" 
                 (Atd_print.string_of_type_name name [expr] ann)) % newline
      % fmt "module %s = struct" (String.capitalize name) % newline
      % indent (
        fmt "type %s t = "
          (match param with
           | [] -> ""
           | more ->
             sprintf "(%s)" (List.map param ~f:(sprintf "'%s") 
                             |> String.concat ", "))
        % t
        % newline
        % fmt "let source = " % source % newline
        % fmt "let sink = " % sink % newline
      )
      % fmt "end" % newline
    in
    t



