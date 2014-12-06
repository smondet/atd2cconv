let () = 
  let source = CConv.Source.int_ and sink = CConv.Sink.int_ in
  test_list (source, sink) [ `Circle 3
                           ; `Square 100
                           ; `Rectangle (4,5)
                           ; `Triangle (1,2,3)
                           ; `Rhombus (0,0)
                           ]
let () = 
  let source = CConv.Source.string_ and sink = CConv.Sink.string_ in
  test_list (source, sink) [ `Circle "a"
                           ; `Square "b"
                           ; `Rectangle ("c","d")
                           ; `Triangle ("x","y","z")
                           ; `Rhombus ("m","n")
                           ]
