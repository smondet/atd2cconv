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
let not_implemented =
  let open Out in
  atom ~t:(string "unit" % space % comment (fmt "not implemented")) ()
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
            let t = string " | " % t in 
            atom ~t ~source:o1 ~sink:i1 ())
    >>= fun ~t ~source ~sink ->
    let t = brakets (indent t) in
    atom ~t ~source:assert_false ~sink:assert_false ()
  | `Tvar (loc, var_name) ->
    let with_mod_name v = 
      string (String.capitalize var_name) % string "." % string v in
    let t = 
      comment (fmt "Tvar %S" var_name) % newline
      % with_mod_name "t" in
    let source = with_mod_name "source" in
    let sink = with_mod_name "sink" in
    atom ~t ~source ~sink ()
  | `Name (_, (loc, t_name, t_args), _) ->
    let with_mod_name v = 
      match t_name, from with
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
      cmtf "`Name -> t_name: %s" t_name
      % (if t = empty then empty else (parens t % space))
      % with_mod_name "t" in
    let source = with_mod_name "source" in
    let sink = with_mod_name "sink" in
    atom ~t ~source ~sink ()

           (*
| `Record of (loc * field list * annot)
| `Tuple of (loc * cell list * annot)
| `List of (loc * type_expr * annot)
| `Option of (loc * type_expr * annot)
| `Nullable of (loc * type_expr * annot)
| `Shared of (loc * type_expr * annot)
| `Wrap of (loc * type_expr * annot)
| `Name of (loc * type_inst * annot)
  *)
  | other -> 
    not_implemented
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
           | more -> sprintf "(%s)" (String.concat ", " param))
        % t
        % newline
        % fmt "let source = " % source % newline
        % fmt "let sink = " % sink % newline
      )
      % fmt "end" % newline
    in
    t

let go_atd atd =
  let (full_module, original_types) = atd in
  let (head, body) = full_module in
  let sorted = Atd_util.tsort body in
  say "sorted: %d elements" (List.length sorted);
  let doc =
    Out.(separate (newline)
           (List.map body transform_moditem)) in
  let filename = "test_result.ml" in
  let o = open_out filename in
  output_string o "module AnotherModule = struct\n\
                  \  type t\n\
                  \  let source = assert false\n\
                  \  let sink = assert false\n\
                   end\n\n";
  Out.to_out_channel 78 2 o doc;
  close_out o;
  let cmd fmt = ksprintf (fun s -> ignore (Sys.command s)) fmt in
  cmd "ocamlfind ocamlc -package cconv -c %s" filename;
  cmd "sed = %s | sed 'N;s/\\n/\\  /'" filename;
  ()
  (* List.map original_types (fun ( *)

let sample = 
"
type ext_type <ocaml from=\"AnotherModule\"> = abstract
type simple_record = {
one: string;
two: string list;
three: [Int of int | Float of float];
}
type simple_variant = [
| One
| Two of simple_record
| Rec of simple_variant
| Double of (int * string) list
]
type inheriting = [
  | One of int
  | inherit simple_variant
]
"

let atd =
  Atd_util.load_string sample

let () = go_atd atd


