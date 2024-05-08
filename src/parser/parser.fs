namespace parser

module Parser =
  open System.Text;
  open BaseParsers
  open FParsec
  open AST
  open ExprParser
  
  
  type Parser<'t> = Parser<'t, unit>

  let ws = spaces
  let str s : Parser<string> = ws >>. pstring s .>> ws 

  let statement_p, statement_pRef =
    createParserForwardedToRef<AST.Statement, unit>()

  let expr_p = ExprParser.expr_p 
  let assign_p =
    let assign3_p =
      tuple3
        (choice Base.dtParsers)
        Base.var_p
        (str "=" >>. expr_p .>> ws .>> Base.terminator_p) |>> AST.Assign3
    let assign2_p =
      tuple2 
        Base.var_p
        (str "=" >>. expr_p .>> ws .>> Base.terminator_p) |>> AST.Assign2
    assign3_p <|> assign2_p


  let if_p =
    tuple3
      (str "Если" >>. str "(" >>. expr_p .>> str ")")
      (between (str "{") (str "}") (many (ws >>. statement_p .>> ws)))
      (opt (str "Иначе" >>. (between (str "{") (str "}") (many (ws >>. statement_p .>> ws)))))
    |>> AST.If

  let while_p =
    tuple2
      (str "Пока" >>. between (str "(") (str ")") expr_p)
      (between (str "{") (str "}") (many (ws >>. statement_p .>> ws)))
    |>> AST.While

  let func_p = 
    tuple3
      (str "Пятилетка" >>. Base.var_p .>> ws)
      (between (str "(") (str ")")
        (sepBy (ws >>. (choice Base.dtParsers) .>>. Base.var_p .>> ws) (str ",")))
      (between (str "{") (str "}")
        (many (ws >>. statement_p .>> ws)))
    |>> AST.Func

  let return_p = 
    str "Сдать" >>. expr_p .>> Base.terminator_p |>> AST.Return
        
  do statement_pRef.Value <- (choice [
      Base.terminator_p
      func_p
      return_p
      if_p
      while_p
      (attempt assign_p)
      expr_p |>> AST.Expr
    ])

  let lexAnalysis program = 
    match run (many (ws >>. statement_p .>> ws)) program with
    | Success(res, _, _) ->  res
    | Failure(errMsg, _, _) -> failwithf "Error: %A" (errMsg)
