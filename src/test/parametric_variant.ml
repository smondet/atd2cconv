(* Testing parametric variants. *)
let () = 
  let source = CConv.Source.int_ and sink = CConv.Sink.int_ in
  test_list (source, sink) [ `Circle 3
                           ; `Square 100
                           ; `Rectangle (4,5)
                           ]
let () = 
  let source = CConv.Source.string_ and sink = CConv.Sink.string_ in
  test_list (source, sink) [ `Circle "a"
                           ; `Square "b"
                           ; `Rectangle ("c","d")
                           ]
