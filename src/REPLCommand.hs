
module REPLCommand where

import Lab2
import Control.Applicative (many, (<|>))
data REPLCommand
  = Quit
  | Load String
  | Eval String



quit :: Parser REPLCommand
quit = (symbol ":quit" <|> symbol ":q") *> pure Quit

load :: Parser REPLCommand
load
  = do
    symbol ":load" <|> symbol ":l"
    f <- many anychar
    return $ Load f

eval :: Parser REPLCommand
eval = Eval <$> many anychar

replEval :: Parser REPLCommand
replEval = Eval <$> many anyChar

replCommand :: Parser REPLCommand
replCommand = quit <|> load <|> eval

