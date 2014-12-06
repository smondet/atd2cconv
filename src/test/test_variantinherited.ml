(* Testing inherited variants. *)
let () = test_list [ `One
                   ; `Two (-100)
                   ; `Three 3.1415
                   ; `Four "foo"
                   ; `Five false
                   ]

