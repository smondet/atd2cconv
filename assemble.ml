open Assemblage
open Printf

let lib_atd2cconv =
  lib "atd2cconv"
    ~deps:[pkg "atd"; pkg "nonstd"; pkg "smart_print"]
    (`Units [
        unit "atd2cconv" (`Path ["src/lib"]);
      ])

let app = 
  bin "atd2cconv"
    ~install:true
    ~deps:[lib_atd2cconv; pkg "atd"; pkg "nonstd"; pkg "smart_print"]
    (`Units [
        unit "main" (`Path ["src/app"]);
      ])

let main_test ~inherit_variants =
  let temp = Filename.temp_file "atd2cconvtest" ".ml" in
  let temp2 = Filename.temp_file "atd2cconvtest" ".ml" in
  let args resolver = [
    "-i"; "src/test/test1.atd";
    "-o"; temp;
    "-inline-inherit-variants"; string_of_bool inherit_variants;
  ] in
  test (sprintf "Test-%b" inherit_variants) ~deps:[app] [
    test_bin app ~args ();
    test_shell "cat src/test/test1_header.ml %s > %s src/test/test1_footer.ml"
      temp temp2;
    (* test_shell "sed = %s | sed 'N;s/\\n/\\  /'" temp2; *)
    test_shell
      "ocamlfind ocamlc -package cconv.yojson -linkpkg %s -o _build/bouh" temp2;
    test_shell "_build/bouh";
    test_shell "echo 'OK inherit-variants: %b, temp: %s'" inherit_variants temp2;
  ] 

let () =
  assemble (project "atd2cconv" [
      lib_atd2cconv;
      app;
      main_test ~inherit_variants:true;
      main_test ~inherit_variants:false;
    ])

