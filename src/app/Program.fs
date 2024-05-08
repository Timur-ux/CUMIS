open evaluator.Evaluator
open parser.Parser
open System.IO
open logger

[<EntryPoint>]
let main args =
  let programFileName = args[0]

  if (File.Exists(programFileName)) then
    let program = File.ReadAllText programFileName

    let ast = lexAnalysis program
    Logger.Log (sprintf "AST: %A\n" (ast))

    let evaluatedAST = eval_AST ast (NamesTable(env_t[], addresses_t[]))
    Logger.Log (sprintf "Evaluating result: %A\n" (evaluatedAST))
    0
  else
    failwithf "Error: program file %A doesn't exist" (programFileName)

