open Assemblage

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

let main_test =
  let temp = Filename.temp_file "atd2cconvtest" ".ml" in
  let temp2 = Filename.temp_file "atd2cconvtest" ".ml" in
  test "T" ~deps:[app] [
    test_bin app ~args:(fun resolver -> ["src/test/test1.atd"; temp]) ();
    test_shell "cat src/test/test1_header.ml %s > %s src/test/test1_footer.ml"
      temp temp2;
    test_shell "sed = %s | sed 'N;s/\\n/\\  /'" temp2;
    test_shell "ocamlfind ocamlc -package cconv.yojson -linkpkg  %s -o _build/bouh" temp2;
    test_shell "_build/bouh ; echo OK";
  ] 

let () =
  assemble (project "atd2cconv" [lib_atd2cconv; app; main_test])

