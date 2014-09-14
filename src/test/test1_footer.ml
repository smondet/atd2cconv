
open Printf
let say fmt = ksprintf (printf "%s\n%!") fmt
let () =
  let json =
    CConv.into Tuple.source CConvYojson.sink 
      (
        42, 
        `Ok ([|
            `Two ({Simple_record. one = "oooooo"; two = Some [| "aaa"; "bbb" |];
                   three = `Float 3.14}, 43);
            `One_more (Some 42);
          |]),
        AnotherModule.(Lambda ("x", App (Lambda ("y", App (Var "y", Var "x")), Var "x")))
      )
  in
  let s = Yojson.Basic.to_string ~std:true json in
  say "%s" s
  
