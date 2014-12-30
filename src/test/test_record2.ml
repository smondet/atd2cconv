(* Testing records of non-uniform types. *)
open Shape_record
let () = 
  test_list [ { circle = "a"; square = true; rectangle = (4.0,5.0) }
            ; { circle = "b"; square = false; rectangle = (0.0,0.0) }
            ]
