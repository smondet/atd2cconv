open Nonstd
open Printf

let say fmt = Printf.(ksprintf (eprintf "%s\n%!") fmt)
let failwithf fmt = Printf.(ksprintf failwith fmt)

let () =
  let inherit_variants = ref true in
  let sort = ref true in
  let input = ref None in
  let output = ref None in
  let options = Arg.(align [
      "-inline-inherit-variants",
      Bool (fun v -> inherit_variants := v),
      "<true|false> \n\tWrite inheriting variants inline (default: true)";
      "-sort",
      Bool (fun v -> sort := v),
      "<true|false> \n\tSort the type-definitions (with `Atd_util.tsort`, \
       default: true)";
      "-i",
      String (fun s -> input := Some s),
      "<file> \n\tInput ATD file (default: stdin)";
      "-o",
      String (fun s -> output := Some s),
      "<file> \n\tOutput ML file (default: stdout)";
    ]) in
  let annons s = say "Dunno what to do with %S" s in
  let usage =
    sprintf "%s [OPTIONS] [-i <input.atd>] [-o <output.ml>]"
      Sys.argv.(0) in
  Arg.parse options annons usage;
  let (i, closi), (o, closo) =
    Option.value_map ~default:(stdin, fun _ -> ()) !input 
      ~f:(fun f -> open_in f, close_in),
    Option.value_map ~default:(stdout, fun _ -> ()) !output 
      ~f:(fun f -> open_out f, close_out) in
  let atd = 
    Atd_util.read_channel 
      ~inherit_fields:true
      ~inherit_variants:!inherit_variants i in
  let (full_module, original_types) = atd in
  let (head, body) = full_module in
  let to_process =
    if !sort then Atd_util.tsort body else [true, body] in
  let doc =
    let open SmartPrint in
    try
      List.fold to_process ~init:(empty) ~f:(fun prev (recur, mod_body) ->
          prev ^^ string "module rec "
          ^^ separate (newline ^^ string "and" ^^ newline)
            (List.map mod_body ~f:(fun item ->
                 match Atd2cconv.transform_module_item item with
                 | `Ok doc -> doc
                 | `Error (`Not_implemented msg) ->
                   failwithf "Error: %S not implemented" msg)))
      ^^ newline
    with e ->
      closi i;
      closo o;
      raise e
  in
  SmartPrint.to_out_channel 78 2 o doc;
  output_string o "\n\n";
  closi i;
  closo o;
  ()



