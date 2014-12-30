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

module Tests = struct

    let test_dir = "src/test"

    let all_test_keys =
      [ "test_unit"
      ; "test_bool"
      ; "test_float"
      ; "test_int"
      ; "test_string"
      ; "test_opt"
      ; "parametric_opt"
      ; "test_list"
      ; "parametric_list"
      ; "test_variant"
      ; "parametric_variant"
      ; "test_variantinherited"
      ; "test_record"
      ; "test_record2"
      ; "parametric_record"
      ; "parametric_recordinherited"
      ; "test_recordinherited"
      ]

    let to_def_file k = Filename.concat test_dir (sprintf "%s.atd" k)

    let to_test_file k = Filename.concat test_dir (sprintf "%s.ml" k)

    let gen_test key ~inherit_variants =
      let prefix = String.sub key 0 (String.index key '_') in
      let generated = Filename.temp_file "atd2cconvtest_generated" ".ml" in
      let fulltests = Filename.temp_file "atd2cconvtest_testfile" ".ml" in
      let tests_file = to_test_file key in
      let def_file  = to_def_file key in
      let args resolver = [
        "-i"; def_file;
        "-o"; generated;
        "-inline-inherit-variants"; string_of_bool inherit_variants;
      ] in
      test (sprintf "Test-%s-%s-%b" prefix key inherit_variants) ~deps:[app] [
        test_bin app ~args ();
        test_shell "cat %s %s.ml %s > %s"
          generated (Filename.concat test_dir prefix) tests_file fulltests;
        test_shell "ocamlfind ocamlc -package cconv.yojson -linkpkg %s -o _build/bouh"
          fulltests;
        test_shell "_build/bouh";
        test_shell "echo 'OK inherit-variants: %b, fulltests: %s'"
          inherit_variants fulltests;
      ]

    let all_tests =
      all_test_keys
      |> List.fold_left (fun test_list key ->
          let test_file = to_test_file key
          and def_file = to_def_file key in
          if (not (Sys.file_exists test_file)) then
            begin
              eprintf "Ignoring missing tests file %s \n." test_file;
              test_list
            end
          else if not (Sys.file_exists def_file) then
            begin
              eprintf "Ignoring missing def file %s \n." def_file;
              test_list
            end
          else
            gen_test key true :: gen_test key false :: test_list) []

  end

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
    test_shell
      "ocamlfind ocamlc -package cconv.yojson -linkpkg %s -o _build/bouh" temp2;
    test_shell "_build/bouh";
    test_shell "echo 'OK inherit-variants: %b, temp: %s'" inherit_variants temp2;
  ]

let () =
  let comp = [ lib_atd2cconv; app ] @
             Tests.all_tests @ [
             main_test ~inherit_variants:true
             ; main_test ~inherit_variants:false ] in
  assemble (project "atd2cconv" comp)
