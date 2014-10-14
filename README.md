ATD To OCaml+CConv
==================

This a tiny library and an application transforming (a subset of possible)
[ATD](https://github.com/mjambon/atd/blob/master/atd_ast.mli)
type definitions to OCaml modules that define
[CConv](https://github.com/c-cube/cconv) sources and sinks.


Build
-----

    assemblage setup
    make
    make test


Usage
-----

### Library

The library has a single function: `Atd2cconv.transform_module_item`
(see [`atd2cconv.mli`](src/lib/atd2cconv.mli), and its use in
[`main.ml`](src/app/main.ml)):

```ocaml
match Atd2cconv.transform_module_item item with
| `Ok doc ->
  SmartPrint.to_stdout 78 2 doc;
| `Error (`Not_implemented msg) ->
  failwithf "Error: %S not implemented" msg
```

### Application

There are very few options:

    atd2cconv [-inline-inherit-variants (true|false)] -i types.atd -o impl.ml

The default value for `-inline-inherit-variants` is `true`, and this is
recommended; it means that the keyword `inherit` in ATD variant types
descriptions will result in “expanded” variants.

Example
-------

The idea of `atd2cconv` (in contrast to `atdgen`) is to not use `Obj.magic`,
ever, aAnd to make modules and types definitions more “standard” so that they
can be used more uniformly within functors and/or first class modules.

```ocaml
type ext_type <ocaml from="AnotherModule"> = abstract
type simple_record = {
  one: ext_type;
  two: string list option;
  three: [Int of int | Float of float];
}
type simple_variant = [
  | One
  | Two of (simple_record * int)
  | Rec of simple_variant
  | Double of (int * string) list
]
type inheriting = [
  | One_more of int option
  | Rec of simple_variant
  | I of simple_record
  | inherit simple_variant
]
```

will result in (implementations truncated for simplicity):

```ocaml
module rec  Ext_type : sig
  type t = AnotherModule.t
  val source : unit -> t CConv.Source.t
  val sink : unit -> t CConv.Sink.t
end = struct
  type  t = AnotherModule.t
  type  t_alias =  t
  let source : unit -> t CConv.Source.t = (* … *)
  let sink : unit -> t CConv.Sink.t = (* … *)
end
module rec  Simple_record : sig
  type t = {
    one: Ext_type.t;
    two: string list option;
    three: [
        | `Int of int
        | `Float of float
      ];
  }
  val source : unit -> t CConv.Source.t
  val sink : unit -> t CConv.Sink.t
end = struct
  type  t = { (* … *) }
  let source : unit -> t CConv.Source.t = (* … *)
  let sink : unit -> t CConv.Sink.t = (* … *)
end
module rec  Simple_variant : sig
  type t = [
    | `One
    | `Two of (Simple_record.t * int)
    | `Rec of t
    | `Double of (int * string) list
  ]
  val source : unit -> t CConv.Source.t
  val sink : unit -> t CConv.Sink.t
end = struct
  type  t = [ (* … *) ]
  let source : unit -> t CConv.Source.t = (* … *)
  let sink : unit -> t CConv.Sink.t = (* … *)
end
module rec  Inheriting : sig
  type t = [
    | `One_more of int option
    | `I of Simple_record.t
    | `One
    | `Two of (Simple_record.t * int)
    | `Rec of Simple_variant.t
    | `Double of (int * string) list
  ]
  val source : unit -> t CConv.Source.t
  val sink : unit -> t CConv.Sink.t
end = struct
  type  t = [ (* … *) ]
  let source : unit -> t CConv.Source.t = (* … *)
  let sink : unit -> t CConv.Sink.t = (* … *)
end
```

