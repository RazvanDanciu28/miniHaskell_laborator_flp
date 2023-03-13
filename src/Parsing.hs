
module Parsing where

import Exp
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
    ( haskellStyle, LanguageDef )
import Text.ParserCombinators.Parsec.Token
import Control.Applicative (some)

miniHaskellDef :: LanguageDef st
miniHaskellDef = haskellStyle {
    reservedNames = ["let", "letrec", "in"],
    reservedOpNames = ["\\", "->", "="]
}

miniHs :: TokenParser st
miniHs = makeTokenParser miniHaskellDef

testParse :: Parser a -> String -> a
testParse p s
  = case parse p "<input>" s of
      Left err -> error (show err)
      Right a -> a

var :: Parser Var
var = Var <$> identify miniHs <|>operations miniHs
-- >>> testParse var "b is a var"
-- Var {getVar = "b"}

varExp :: Parser ComplexExp
varExp = undefined
-- >>> testParse varExp "b is a var"
-- CX (Var {getVar = "b"})

lambdaExp :: Parser ComplexExp
lambdaExp = undefined
-- >>> testParse lambdaExp "\\x -> x"
-- CLam (Var {getVar = "x"}) (CX (Var {getVar = "x"}))

letExp :: Parser ComplexExp
letExp = undefined
-- >>> testParse letExp "let x := y in z"
-- Let (Var {getVar = "x"}) (CX (Var {getVar = "y"})) (CX (Var {getVar = "z"}))

letrecExp :: Parser ComplexExp
letrecExp = undefined
-- >>> testParse letrecExp "letrec x := y in z"
-- LetRec (Var {getVar = "x"}) (CX (Var {getVar = "y"})) (CX (Var {getVar = "z"}))

listExp :: Parser ComplexExp
listExp = undefined
-- >>> ghci> testParse listExp "[a,b,c]"
-- List [CX (Var {getVar = "a"}),CX (Var {getVar = "b"}),CX (Var {getVar = "c"})]

natExp :: Parser ComplexExp
natExp = undefined
-- >>> ghci> testParse natExp "223 a"
-- Nat 223

parenExp :: Parser ComplexExp
parenExp = (parens miniHs) expr
-- >>> ghci> testParse parenExp "(a)"
-- CX (Var {getVar = "a"})

basicExp :: Parser ComplexExp
basicExp = undefined
-- >>> testParse basicExp "[a,b,c]"
-- List [CX (Var {getVar = "a"}),CX (Var {getVar = "b"}),CX (Var {getVar = "c"})]

aux :: [ComplexExp] -> ComplexExp
aux [x] = x
aux (x:xs) = CApp x  $ aux xs

expr :: Parser ComplexExp
expr = do
    es <- some (basicExp)
    return $ foldl1 CApp es
-- >>> testParse expr "\\x -> [x,y,z]"
-- CLam (Var {getVar = "x"}) (List [CX (Var {getVar = "x"}),CX (Var {getVar = "y"}),CX (Var {getVar = "z"})])

exprParser :: Parser ComplexExp
exprParser = whiteSpace miniHs *> expr <* eof
-- >>> ghci> testParse exprParser "let x := 28 in \\y -> + x y"
-- Let (Var {getVar = "x"}) (Nat 28) (CLam (Var {getVar = "y"}) (CApp (CApp (CX (Var {getVar = "+"})) (CX (Var {getVar = "x"}))) (CX (Var {getVar = "y"}))))

