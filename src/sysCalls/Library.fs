namespace sysCalls

module sysCalls =
  open AST

  let rec print args =
    match args with
    | h::t ->
      match h with
      | AST.Const(data) ->
        match data with
        | AST.String(str) ->
          printf "%s" (str)
        | AST.Int(n) ->
          printf "%d" (n)
        | AST.Bool(b) ->
          if b then
            printf "Истина"
          else
            printf "Ложь"
        | AST.Float(f) ->
          printf "%f" (f)
      | any -> printf "%A" (any)
      print t
      | [] -> printfn ""
