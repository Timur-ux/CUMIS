namespace BaseParsers

module Base =
  open System.Collections.Generic
  open FParsec
  open AST

  type Parser<'t> = Parser<'t, unit>

  let ws = spaces
  let str s : Parser<string> = ws >>. pstring s .>> ws 

  let dataTypes = ["Цел"; "Дроб"; "Строка"; "Булка"]
  let operators = ["+"; "-"; "*"; "/"; "<"; "<="; ">"; ">="; "=="; "="; "!="]
  let keywords = ["Пятилетка"; "Сдать"; "Если"; "Иначе"; "Пока"; "{"; "}"; ";"; ","; "("; ")"]

  let kwParsers: seq<Parser<string>> = seq {for kw in keywords -> (str kw)}
  let dtParsers: seq<Parser<string>> = seq {for dt in dataTypes -> (str dt)}
  let opParsers: seq<Parser<string>> = seq {for op in operators -> (str op)}

  let baseParsers = (choice kwParsers) <|> (choice dtParsers) <|> (choice opParsers)


  let const_pDict = new Dictionary<string, Parser<AST.DataType>>()
  const_pDict.Add("Цел", (ws >>. pint32 .>> notFollowedBy (str ".") .>> ws |>> AST.Int))
  const_pDict.Add("Дроб", (ws >>. pfloat .>> ws |>> AST.Float))
  const_pDict.Add("Строка", ((str "\"" >>. (manySatisfy (fun c -> c <> '"')) .>> str "\"") |>> AST.String))
  const_pDict.Add("Булка", (ws >>. (stringReturn "Ложь" false) <|> (stringReturn "Истина" true) .>> ws |>> AST.Bool))

  let const_p: Parser<AST.Expr> = 
    choice (seq {for KeyValue(_, v) in const_pDict -> (ws >>. v .>> ws) |>> AST.Const})

  let var_p : Parser<string> =
    let f1 c = (c >= 'а' && c <= 'я') || (c >= 'А' && c <= 'Я') || c = '_'
    let f2 c = (isDigit c) || (f1 c)
    (ws >>. (many1Satisfy2 f1 f2) .>> ws) 
  let id_p = var_p |>> AST.Id


  let terminator_p =
    str ";" |>> AST.Terminator

