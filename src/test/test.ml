(* General framework for non-parametric (all types specified) tests.*)
open Printf
let say fmt = ksprintf (printf "%s\n%!") fmt

let testi index value =
  let paired = (index, value) in
  let json = CConv.into (Test_tuple.source ()) CConvYojson.sink paired in
  let s = Yojson.Basic.to_string ~std:true json in
  say "%s" s;
  assert (CConv.from CConvYojson.source (Test_tuple.sink ()) json = paired)

let test_list = List.iteri testi

let test x = test_list [x]
