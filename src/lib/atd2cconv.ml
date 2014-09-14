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
  let apply_1 funname sub = parens (fmt funname % space % parens (sub))
end

let say fmt = Printf.(ksprintf (eprintf "%s\n%!") fmt)


let atom ?(t=Out.empty) ?(source=Out.empty) ?(sink=Out.empty) () =
  `Ok (`T t, `Source source, `Sink sink)

let (>>=) t f =
  match t with
  | `Ok (`T t, `Source source, `Sink sink) -> f ~t ~source ~sink
  | `Error e -> `Error e

(*
let (+++) a b =
  a >>= fun ~t ~source ~sink  ->
  b >>= fun ~t:t2 ~source:o2 ~sink:i2  ->
  let open Out in
  atom ~t:(t % t2) ~source:(source % o2) ~sink:(sink % i2) ()
*)

let empty_atom = atom ()

let not_implemented_placeholer = Out.fmt "Obj.magic 0"
let not_implemented msg =
  `Error (`Not_implemented msg)
    (*
  let open Out in
  atom ~t:(string "unit" % space % comment (fmt "not implemented: %s" msg)) ()
    ~source:not_implemented_placeholer
    ~sink:not_implemented_placeholer *)

let transform_expr ~self_name ?from expr =
  let open Out in
  let rec go_deep = function
  | `Sum (loc, variants, ann) ->
    let backtick n = fmt "`%s" n in
    let variant_case name expr =
      match expr with
      | Some e ->
        e >>= fun ~t ~source ~sink ->
        let t = fmt  "| " % backtick name % fmt " of " % t % newline in
        let source =
          fmt "| " % backtick name % space % fmt "elt -> " % parens (
            OCaml.string name % comma 
            % fmt "hcons" % space % parens source % space % fmt "elt @@ hnil"
          ) % newline
        in
        atom ~t ~source ~sink ()
      | None ->
        let t = fmt  "| " % backtick name % newline in
        let source = fmt "| " % backtick name % fmt " -> "
                     % parens (OCaml.string name % comma % fmt "hnil") in
        atom ~t ~source () in
    let unused_name () =
      let names = List.filter_map variants ~f:(function
        | `Variant (_, (n, _), _) -> Some n
        | `Inherit _ -> None) in
      let rec n t =
        if List.mem t ~set:names then n (sprintf "%s%s" t t) else t in
      n "I"
    in
    List.fold ~init:empty_atom variants ~f:(fun prev v ->
        prev >>= fun ~t:t1 ~source:o1 ~sink:i1  ->
        match v with
        | `Variant (loc, (name, ann), expr_opt) ->
          variant_case name Option.(map expr_opt ~f:go_deep)
          >>= fun ~t ~source ~sink  ->
          atom ~t:(t1 % t) ~source:(o1 % source) ()
        | `Inherit (loc, expr) ->
            go_deep expr >>= fun ~t ~source ~sink ->
            let newt = t1 % string "| " % t % newline in
            let name = OCaml.string (unused_name ()) in
            let source =
              o1 % fmt "| #" % t % fmt " as inher -> " % name % fmt ", hcons " 
              % source % fmt " inher @@ hnil" in
            atom ~t:newt ~source ~sink:i1 ())
    >>= fun ~t ~source ~sink ->
    let t = brakets (newline % indent t) in
    let source = 
      fmt "sum_fix" % parens (newline
        % fmt "fun t -> function" % newline %
        source) in
    atom ~t ~source ~sink:not_implemented_placeholer ()
  | `Tvar (loc, var_name) ->
    let t =
      (* comment (fmt "Tvar %S" var_name) % newline *)
      fmt "'%s" var_name in
    let source = fmt "%s" var_name in
    let sink =  fmt "%s" var_name in
    atom ~t ~source ~sink ()
  | `Name (_, (loc, t_name, t_args), _) ->
    let with_mod_name kind v =
      let ret3strings t source sink =
        match kind with
        | `T -> string t
        | `Source -> string source
        | `Sink -> string sink in
      let same t = t in
      match t_name, from with
      | "int", _ -> ret3strings "int" "int_" "int_"
      | "float", _ -> ret3strings "float" "float_" "float_"
      | "string", _ -> ret3strings "string" "string_" "string_"
      | "list", _ -> ret3strings "list" "list_" "list_"
      | "abstract", Some f ->
        same (string (String.capitalize f) % string "." % string v)
      | other, _ when other = self_name ->
        same (string "t")
      | other, _ ->
        same (string (String.capitalize other) % string "." % string v)
    in
    begin match t_args with
    | [] -> empty_atom
    | one :: more ->
      let init = go_deep one in
      List.fold more ~init ~f:(fun prev arg_expr ->
          prev >>= fun ~t:t1 ~source:o1 ~sink:i1  ->
          go_deep arg_expr >>= fun ~t:t2 ~source:o2 ~sink:i2  ->
          atom ()
            ~t:(t1 % comma % t2)
            ~source:(o1 % space % o2)
            ~sink:(i1 % space % i2))
    end
    >>= fun ~t ~source ~sink  ->
    let t =
      (* commentf "t_name: %s" t_name % space % *)
      (if t = empty then empty else (parens t % space))
      % with_mod_name `T "t" in
    let source = with_mod_name `Source "source" % space % source in
    let sink = not_implemented_placeholer in
    atom ~t ~source ~sink ()
  | `Tuple (loc, [], annot) -> assert false
  | `Tuple (loc, cell :: cell_list, annot) ->
    let (_, expr, _) = cell in
    let hconsify inside index =
      fmt "hcons" % space % parens inside % space % fmt "e%d" index % newline in
    let init = 
      go_deep expr >>= fun ~t ~source ~sink  ->
      atom ~t ~source:(hconsify source 0) ~sink ()
    in
    List.foldi ~init cell_list ~f:(fun index prev (_, expr, _) ->
        prev >>= fun ~t:t1 ~source:o1 ~sink:i1  ->
        go_deep expr >>= fun ~t:t2 ~source:o2 ~sink:i2 ->
        atom ()
          ~t:(t1 % fmt " *" % space % t2)
          ~source:(o1 % fmt " @@ " % hconsify o2 (index + 1))
          ~sink:(i1 % fmt "; " % i2))
    >>= fun ~t ~source ~sink  ->
    let source = 
      fmt "tuple " % parens (
        fmt "fun " % parens (
          List.mapi (cell :: cell_list) ~f:(fun i _ -> fmt "e%d" i) 
          |> separate (fmt ", ")) % fmt " -> " % newline
        % indent (parens (source % fmt " hnil")))
    in
    atom () ~t:(parens t) ~source  ~sink
  | `List (_, expr, _) ->
    go_deep expr
    >>= fun ~t ~source ~sink  ->
    atom ()
      ~t:(t % fmt " array")
      ~source:(apply_1 "array_" source)
      ~sink:(sink )
  | `Option (_, expr, _) ->
    go_deep expr
    >>= fun ~t ~source ~sink  ->
    atom ()
      ~t:(t % fmt " option")
      ~source:(apply_1 "opt" source)
      ~sink:(sink )
  | `Record (_, field_list, _) ->
    List.fold ~init:empty_atom field_list ~f:(fun prev f ->
        prev >>= fun ~t:t1 ~source:o1 ~sink:i1  ->
        match f with
        | `Field (_, (name, kind, _), expr) ->
          go_deep expr >>= fun ~t:t2 ~source:o2 ~sink:i2  ->
          let t = t1 % fmt "%s: " name % indent (t2) % fmt ";" % newline in
          let source =
            o1 % fmt "field " % OCaml.string name % space
            % parens (fmt "fun p -> p." % string name) % space 
            % parens o2 % fmt " @@ "
            % newline
          in
          atom () ~t
            ~source
            ~sink:(i1 % i2 % fmt ";" % newline)
        | `Inherit (_, expr) ->
          failwith "Not implemented: Inherit record fields")
    >>= fun ~t ~source:source_inside ~sink  ->
    let source =
      fmt "record_fix" % space % indent 
        (parens (fmt "fun t ->" % newline % source_inside % fmt "record_stop"))
    in
    atom ()
      ~t:(braces (newline % t |> indent))
      ~source ~sink:(sink % not_implemented_placeholer)
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

let transform_module_item =
  let open Out in
  function
  | `Type (loc, (name, param, ann), expr) ->
    let from = get_from_annotation ann in
    let self_name = name in
    begin match transform_expr ?from ~self_name expr with
    | `Ok (`T t, `Source source, `Sink sink) ->
      let type_parameters =
        match param with
        | [] -> ""
        | more ->
          sprintf "(%s)" (List.map param ~f:(sprintf "'%s")
                          |> String.concat ", ")
      in
      let function_type modname =
        match param with
        | [] -> fmt "t CConv.%s.t" modname
        | more ->
          separate (fmt " -> ")
            (List.map param ~f:(fun n -> fmt "'%s CConv.%s.t" n modname))
          % fmt " -> %s t CConv.%s.t" type_parameters modname
      in
      let fun_definition =
        match param with
        | [] -> fmt ""
        | more ->
          fmt "fun " % separate space (List.map more (fmt "%s")) % fmt " -> "
      in
      let t_sink_source what inside =
        let modname = String.capitalize what in
        fmt "let %s : " what % function_type modname % fmt " =" % newline
        % indent (fun_definition % fmt "CConv.%s." modname % parens inside)
        % newline in
      let t =
        comment (fmt "Type %s"
                   (Atd_print.string_of_type_name name [expr] ann)) % newline
        % fmt "module %s = struct" (String.capitalize name) % newline
        % indent (
          fmt "type %s t = " type_parameters % t
          % newline
          % t_sink_source "source" source
          % t_sink_source "sink" sink
        )
        % fmt "end" % newline
      in
      `Ok t
    | `Error e -> `Error e
    end



