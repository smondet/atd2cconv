(* General test methods of parametric ATD (with respect to file 
   definitions) examples. *)
open Printf
let say fmt = ksprintf (printf "%s\n%!") fmt

let testi (source, sink) index value =
  let paired = (index, value) in
  let json = CConv.into (Test_tuple.source source ()) CConvYojson.sink paired in
  let s = Yojson.Basic.to_string ~std:true json in
  say "%s" s;
  assert (CConv.from CConvYojson.source (Test_tuple.sink sink ()) json = paired)

let test_list p = List.iteri (testi p)

let test p x = test_list p [x]
