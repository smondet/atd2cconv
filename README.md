ATD To OCaml+CConv
==================

This a tiny library and an application transforming (a subset of possible)
[ATD](https://github.com/mjambon/atd/blob/master/atd_ast.mli)
type definitions to OCaml modules that define
[CConv](https://github.com/c-cube/cconv) sources and sinks.

**Warning:** This version `0.0.0` generates code for `cconv` version **0.1**.


Build
-----

Just type:

    make
    make install BINDIR=/some/where/bin


Usage
-----

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

The application:

    atd2cconv [-inline-inherit-variants (true|false)] -i types.atd -o impl.ml


