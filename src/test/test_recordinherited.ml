(* Testing inherited records. *)
open Shape_record
let () =
  test_list [ { circle = 3.0; square = 100.0; rectangle = (4.0,5.0)
              ; triangle = (1.0,2.0,3.0); rhombus = (0.1,0.2)}
            ; { circle = -30.0; square = -10.0; rectangle = (0.0,0.0)
              ; triangle = (1.1,2.1,3.1); rhombus = (1.1,1.2)}
            ]
