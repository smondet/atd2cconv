(* Testing parametric lists. *)
let () =
  let source = CConv.Source.unit_ and sink = CConv.Sink.unit_ in
  test_list (source, sink) [ [] ; [()] ; [();();();] ]

let () =
  let source = CConv.Source.bool_ and sink = CConv.Sink.bool_ in
  test_list (source, sink) [ [] ; [true] ; [false]; [true;false;false]]

let () =
  let source = CConv.Source.int_ and sink = CConv.Sink.int_ in
  test_list (source, sink) [ [] ; [100] ; [-1;100;1;2;3;4;5] ]

let () =
  let source = CConv.Source.float_ and sink = CConv.Sink.float_ in
  test_list (source, sink) [ [] ; [100.0] ; [-1.0;100.0;1.0;2.0;3.0;4.0;5.0] ]

let () = 
  let source = CConv.Source.string_ and sink = CConv.Sink.string_ in
  test_list (source, sink) [ [] ; ["apple"] ; ["foo";"bar";"dog"] ]
