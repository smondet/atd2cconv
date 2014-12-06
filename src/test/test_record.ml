(* Tesing records *)
open Shape_record
let () = 
  test_list [ { circle = 3.0; square = 100.0; rectangle = (4.0,5.0) }
            ; { circle = -30.0; square = -10.0; rectangle = (0.0,0.0) }
            ]
