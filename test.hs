import RSXP


getAST = do
     str <- readFile "test.xml"
     let ast = parseXML str
     return ast

main = do
     str <- readFile "test.xml"
     let ast = parseXML str
     putStrLn (show ast)