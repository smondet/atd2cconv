open Nonstd
open Printf

let say fmt = Printf.(ksprintf (eprintf "%s\n%!") fmt)

let () =
  match Array.to_list Sys.argv with
  | _ :: [input; output] ->
    let atd = Atd_util.load_file input in
    let (full_module, original_types) = atd in
    let (head, body) = full_module in
    let doc =
      SmartPrint.(separate (newline)
                    (List.map body ~f:Atd2cconv.transform_module_item)) in
    let o = open_out output in
    SmartPrint.to_out_channel 78 2 o doc;
    close_out o;
    ()
  | other ->
    say "error: wrong command line";
    say "usage: %s <input-file> <output-file>" Sys.argv.(0);
    ()



