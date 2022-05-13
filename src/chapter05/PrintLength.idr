

printLength : IO ()
printLength = do putStr "Input string: "
                 input <- getLine
                 let len = length input
                 putStrLn (show len)

printLonger : IO ()
printLonger = do putStr "Input string #1: "
                 in1 <- getLine
                 putStr "Input string #2: "        
                 in2 <- getLine
                 let len1 = length in1
                 let len2 = length in2
                 let len = if len1 > len2 then len1 else len2 
                 putStrLn (show len)

printLonger2 : IO ()
printLonger2 = putStr "Input string #1: " >>= \_ =>
               getLine >>= \input =>
               putStr "Input string #2: " >>= \_ =>
               getLine >>= \input2 =>
               let len1 = length input
                   len2 = length input2
                   len = if len1 > len2 then len1 else len2 in
               putStrLn (show len)
