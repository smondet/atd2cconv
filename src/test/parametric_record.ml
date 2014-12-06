(* Testing parametric records. *)
open Shape_record
let () = 
  let source = CConv.Source.int_ and sink = CConv.Sink.int_ in
  test (source, sink) { circle = 3; square = 100; rectangle = (4,5) }

let () = 
  let source = CConv.Source.string_ and sink = CConv.Sink.string_ in
  test (source, sink) { circle = "a"; square = "b"; rectangle = ("c","d") }
