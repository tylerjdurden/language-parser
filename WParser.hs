module WParser ( parse,
                 wprogram ) where

  import Data.Char
  import Control.Applicative (Applicative(..))
  import Control.Monad       (liftM, ap)
  import W

  -----------------------------
  -- This is the main parser --
  -----------------------------
  wprogram = whitespace >> many stmt >>= \ss -> return $ Block ss
  -- a program is a sequence of statements; the parser returns them
  -- as a single block-statement

  -- only two of the statement types below are supported, 
  -- the rest are undefined.
  -- please implement them
  stmt = varDeclStmt +++ assignStmt +++ ifStmt +++ whileStmt +++ 
         blockStmt +++ emptyStmt +++ printStmt


  printStmt = do
    keyword "print"
    e <- expr
    symbol ";"
    return $ Print e

  emptyStmt = do 
    symbol ";" 
    return Empty

  varDeclStmt = do
    keyword "var"
    varname <- identifier
    symbol "="
    varvalue <- expr
    symbol ";"
    return $ VarDecl varname varvalue

  assignStmt = do
    varname <- identifier
    symbol "="
    varvalue <- expr
    symbol ";"
    return $ Assign varname varvalue

  ifStmt = do
    keyword "if"
    cond <- parens expr 
    thenstmt <- stmt
    elsestmt <- stmt
    return $ If cond thenstmt elsestmt

  whileStmt = do
    keyword "while"
    cond <- parens expr
    thenstmt <- stmt
    return $ While cond thenstmt

  blockStmt = do
    symbol "{"
    statements <- many stmt
    symbol "}"
    return $ Block statements

  -- the only kind of expression supported for now is stringLiterals
  -- implement the full expression language of W
  --
  -- expr = stringLiteral 

  expr = term >>= termSeq
  termSeq left =
      ( do 
            op <- (symbol "+" >> return Plus) +++ (symbol "-" >> return Minus)
            right <- term
            termSeq (op left right)
      ) +++ return left

  term = factor >>= factorSeq
  factorSeq left =
      ( do 
            op <- (symbol "*" >> return Multiplies) +++ (symbol "/" >> return Divides)
            right <- factor
            factorSeq (op left right)
      ) +++ return left

  factor = (nat >>= \n -> return $ Val (VInt n))
    +++ stringLiteral +++ parens expr

  -- stringLiterals can contain \n characters
  stringLiteral = do char ('"') 
                     s <- many stringChar
                     char ('"')
                     whitespace
                     return $ Val (VString s)

  stringChar = do ( char '\\' >> char 'n' >> return '\n' ) 
               +++ sat (/= '"')

  ----------------------
  -- Parser utilities --
  ----------------------

  keywords = words "var if else while"
  isKeyword s = s `elem` keywords

  keyword s = do 
    s' <- identifier 
    if s' == s then return s else failure     
         
  newtype Parser a = P (String -> [(a, String)])
    
  parse :: Parser a -> String -> [(a, String)]
  parse (P p) inp = p inp
    
  instance Monad Parser where
      -- return :: a -> Parser a
      return v = P $ \inp -> [(v, inp)]
    
      -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
      p >>= q = P $ \inp -> case parse p inp of 
                              [] -> []
                              [(v, inp')] -> let q' = q v in parse q' inp'
    
  instance Functor Parser where
    fmap = liftM

  instance Applicative Parser where
    pure = return
    (<*>) = ap

  failure :: Parser a
  failure = P $ \_ -> []
    
  item :: Parser Char 
  item = P $ \inp -> case inp of 
                       (x:xs) -> [(x, xs)]
                       [] -> []
    
  -- Parse with p or q
  (+++) :: Parser a -> Parser a -> Parser a
  p +++ q = P $ \inp -> case parse p inp of 
                          [] -> parse q inp
                          [(v, inp')] -> [(v, inp')]
    
    
  -- Simple helper parsers
  sat :: (Char -> Bool) -> Parser Char
  sat pred = do c <- item 
                if pred c then return c else failure
    
  digit, letter, alphanum :: Parser Char
  digit = sat isDigit
  letter = sat isAlpha
  alphanum = sat isAlphaNum
    
  char :: Char -> Parser Char
  char x = sat (== x)
    
  string = sequence . map char 
    
  many1 :: Parser a -> Parser [a]
  many1 p = do v <- p 
               vs <- many p 
               return (v:vs)
    
  many :: Parser a -> Parser [a]
  many p = many1 p +++ return []
    
  -- Useful building blocks
  nat :: Parser Int
  nat = do s <- many1 digit 
           return (read s)
    
  identifier :: Parser String
  identifier = do s <- letter
                  ss <- many alphanum
                  whitespace
                  return (s:ss)

  whitespace :: Parser ()
  whitespace = many (sat isSpace) >> comment
    
  symbol s = do 
    s' <- string s
    whitespace
    return s'    
    
  comment = ( do string "//" 
                 many (sat (/= '\n')) 
                 whitespace ) +++ return ()
  parens p = do 
    symbol "("
    p' <- p
    symbol ")"
    return p'
