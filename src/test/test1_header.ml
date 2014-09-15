module AnotherModule = struct
  type t =
    | Var of string
    | App of t * t
    | Lambda of string * t

  let source () = CConv.Source.(sum_fix
    (fun self t -> match t with
        | Var s -> "var", hcons string_ s @@ hnil
        | App (t1, t2) -> "app", hcons self t1 @@ hcons self t2 @@ hnil
        | Lambda (s, t) -> "lam", hcons string_ s @@ hcons self t @@ hnil
      ))

  let sink () = CConv.Sink.(sum_fix
    (fun self str -> match str with
      | "var" -> string_ |+| fun s -> yield (Var s)
      | "app" -> self |+| fun t1 -> self |+| fun t2 -> yield (App (t1, t2))
      | "lam" -> string_ |+| fun s -> self |+| fun t -> yield (Lambda (s, t))
      | _ -> CConv.report_error "expected lambda term"
    ))
end

