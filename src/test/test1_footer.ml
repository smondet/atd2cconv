
open Printf
let say fmt = ksprintf (printf "%s\n%!") fmt
let () =
  let value =
    (
      42, 
      `Ok ([
          `Two ({Simple_record. one = "oooooo"; two = Some [ "aaa"; "bbb" ];
                 three = `Float 3.14}, 43);
          `One_more (Some 42);
        ]),
      AnotherModule.(Lambda ("x", App (Lambda ("y", App (Var "y", Var "x")), Var "x")))
    ) 
  in
  let json = CConv.into (Tuple.source ()) CConvYojson.sink  value in
  let s = Yojson.Basic.to_string ~std:true json in
  say "%s" s;
  assert (CConv.from CConvYojson.source (Tuple.sink ()) json = value)
  
