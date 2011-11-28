import MGDS



sampleFactorialProgam :: Program
sampleFactorialProgam = 
  Program $
  (Function "fact" ["n"] $ If
      (Less (Var "n") (Constant 2))
      (Constant 1) $
      FunctionCall "fact" [Subtract (Var "n") (Constant 1)])
  : (Function "main" [] $ FunctionCall "fact" [Constant 6])
  : []
  
main = do
  return ()