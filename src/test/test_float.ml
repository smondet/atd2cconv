(*Testing floats *)
let () = test_list [1.0; 2.0; -10.0; 300.0
                   ; float_of_int max_int
                   ; float_of_int min_int
                   (* Unacceptable for JSON
                   ; nan 
                   ; infinity
                   ; neg_infinity
                   *)
                   ]
