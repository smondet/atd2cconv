open Nonstd
open Printf

let say fmt = Printf.(ksprintf (eprintf "%s\n%!") fmt)
let failwithf fmt = Printf.(ksprintf failwith fmt)

let () =
  match Array.to_list Sys.argv with
  | _ :: [input; output] ->
    let atd = Atd_util.load_file ~inherit_variants:false input in
    let (full_module, original_types) = atd in
    let (head, body) = full_module in
    let o = open_out output in
    List.iter body ~f:(fun item ->
        match Atd2cconv.transform_module_item item with
        | `Ok doc ->
          SmartPrint.to_out_channel 78 2 o doc;
          output_string o "\n\n"
        | `Error (`Not_implemented msg) ->
          close_out o;
          failwithf "Error: %S not implemented" msg
      );
    close_out o;
    ()
  | other ->
    say "error: wrong command line";
    say "usage: %s <input-file> <output-file>" Sys.argv.(0);
    ()



