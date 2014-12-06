(* TODO (Leonid), It is yet to be determined how to implement nullable in OCaml,
 * but since it will almost certainly be something like converting to wrapping
 * type in an option (maybe not for floats?), I'll leave this as an example.
 *)
let () =
  let source = CConv.Source.unit_ and sink = CConv.Sink.unit_ in
  test_list (source, sink) [ None ; Some () ]

let () =
  let source = CConv.Source.bool_ and sink = CConv.Sink.bool_ in
  test_list (source, sink) [ None ; Some true; Some false ]

let () =
  let source = CConv.Source.int_ and sink = CConv.Sink.int_ in
  test_list (source, sink) [ None ; Some 100 ; Some (-1) ]

let () =
  let source = CConv.Source.float_ and sink = CConv.Sink.float_ in
  test_list (source, sink) [ None ; Some 100.0 ; Some (-1.0) ]

let () = 
  let source = CConv.Source.string_ and sink = CConv.Sink.string_ in
  test_list (source, sink) [ None ; Some "apple" ; Some "pie" ]
