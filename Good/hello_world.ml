open StdLib

main = begin
    putStrLn "Hello World!"
    putStrLn "What's your name?"
    name : String <- getLine
    putStrLn <| "Nice to meet you, " ++ name ++ "!"
  end
