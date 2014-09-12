open Assemblage

let app = 
  bin "atd2cconv"
    ~install:true
    ~deps:[pkg "atd"; pkg "nonstd"; pkg "smart_print"]
    (`Units [
        unit "main" (`Path ["src/app"]);
      ])

let main_test =
  test "T" ~deps:[app]
    [test_bin app ()] 

let () =
  assemble (project "atd2cconv" [app;main_test])

