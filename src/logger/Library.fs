namespace logger

module Logger =
  open System.IO
  
  let mutable firstLog = true
  let Log (s: string) = 
    task{
      use logger = File.Open("./CompileLog.log", FileMode.Append)
      if firstLog then
        do! logger.WriteAsync(System.Text.Encoding.UTF8.GetBytes(
            "----------------------------------------\n"
        )) 
        firstLog <- false
        do! logger.WriteAsync(System.Text.Encoding.UTF8.GetBytes(s)) 
      else
        do! logger.WriteAsync(System.Text.Encoding.UTF8.GetBytes(s)) 
    } |> ignore
    ()
