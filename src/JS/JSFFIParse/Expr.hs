{-# LANGUAGE TupleSections #-} 

module JS.JSFFIParse.Expr where

import JS.Types (Name)

import Text.Parsec

data DotRef a = Property Name | Fn (Function a) 
data Ref' a = Prop Name | FnCall Name [(Expr a, [Dependency])] --(Function a)
type RefChain a = [Ref' a] -- f(1).x(2).y AND `name` 


type Dependency = Name

type Expr' a = (Expr a, [Dependency])

-- Anything which can be named 
data Expr a = Val (JSValue a)
            | Reference [Ref' a] -- decl. function app and/or object reference
            | Op Operator (Expr a) (Expr a) --(RefChain a)
            | AnonFunc (Function a)
            | ApplyFunc (Function a) [Expr a]
            | New Name [Expr a]
            -- var a = function f(x) { ... }
            -- note that f is not defined 


-- Note that these can literally pair with any two JS Values 
data Operator = Multiply
              | Plus
              | Subtract
              | Exponentiation
              | Division
              | Modulo
              | Equal -- (==)
              | EqualSameType  -- (===)
              | NotEqual
              | NotEqualOrNotSameType
              | GreaterThan
              | LessThan
              | LessOrEqual
              | GreaterOrEqual
              | Ternary -- ? ~~ isTrue
              | AND
              | OR
              | NOT
              | ANDB
              | ORB
              | NOTB
              | XORB
              | Shift String 



-- as this recurses, we will need to add dependencies
-- | Actually this is only named expressions: for and while cannot be in this named context
jsExpression :: (Fractional a, Read a, Stream s m Char) => {-Maybe Name ->-} ParsecT s u m ((Expr a), [Dependency])
jsExpression = do
  -- TODO(galen): OR this can just be a function and anonymous function: var x = function(x){}
  (try objectInstantiation) <|> (try $ coerciblyAnonFunc) <|> arithmeticExpression

  where
    -- coercibly becuase the name is not available 
    coerciblyAnonFunc = do
      -- in the case
      undefined

        -- has a dependency by default 
    objectInstantiation = do
      string "new"
      many (char ' ')
      nameRefd <- jsValidName
      argsAndDeps <- jsArgTupleInput
      pure $ (New nameRefd (fmap fst argsAndDeps), nameRefd : (mconcat $ fmap snd argsAndDeps))

    arithmeticExpression = do 
  
      (expr,deps) <- whnfValue
      many (char ' ')
      mComp <- (Nothing <$ oneOf ['\n', ';']) <|> (Just <$> do
                                                      combinator <- someOperator
                                                      many (char ' ') 
                                                      exprANDdep <- jsExpression
                                                      pure (combinator, exprANDdep)
                                                  )
      case mComp of
        Nothing -> pure (expr, deps)
        Just (combinator, (expr2,deps2)) -> pure (Op combinator expr expr2, deps <> deps2)
    
  
    whnfValue = (,[]) <$> (Val <$> jsValue)
                <|> existingConstructRef

    
    existingConstructRef :: (Fractional a, Read a, Stream s m Char) => ParsecT s u m ((Expr a), [Dependency])
    existingConstructRef = do -- Reference <$> (sepBy (name <|> nameWithArgs) (char '.'))
      (refs, deps) <- unzip <$> (sepBy (name <|> nameWithArgs) (char '.'))
      pure (Reference $ refs, mconcat deps)
        where
          name = do
            n <- jsValidName
            pure (Prop n, [n]) 
          nameWithArgs = do
            (n, argExprWithDeps) <- parseCallsWith
            pure $ (FnCall n argExprWithDeps, mconcat $ fmap snd argExprWithDeps) 


someOperator :: Stream s m Char => ParsecT s u m Operator
someOperator =
  (Multiply <$ (string "*"))
  <|> (Plus <$ (string "+"))
  <|> (Subtract <$ (string "-"))
  <|> (Exponentiation <$ (string "**"))
  <|> (Division <$ (string "/"))
  <|> (Modulo <$ (string "%"))
  <|> (Equal <$ (string "=="))
  <|> (EqualSameType <$ (string "==="))
  <|> (NotEqual <$ (string "!="))
  <|> (NotEqualOrNotSameType <$ (string "!=="))
  <|> (GreaterThan <$ (string ">"))
  <|> (LessThan <$ (string "<"))
  <|> (LessOrEqual <$ (string "<="))
  <|> (GreaterOrEqual <$ (string ">="))
  <|> (Ternary <$ (string "?"))
  <|> (AND <$ (string "&&"))
  <|> (OR <$ (string "||"))
  <|> (NOT <$ (string "!")) -- Should i make the second Expr a Maybe Expr? I may have to
  <|> (ANDB <$ (string "&"))
  <|> (ORB <$ (string "|"))
  <|> (NOTB <$ (string "~"))
  <|> (XORB <$ (string "^"))
  <|> (Shift <$> shift)
  where
    shift = (try $ string ">>") <|> (try $ string "<<") <|> (try $ string ">>>")


parseCallsWith :: (Fractional a, Read a, Stream s m Char) => ParsecT s u m (Name, [(Expr a, [Dependency])])
parseCallsWith = do
  refdName <- jsValidName
  args <- option [] jsArgTupleInput
  pure (refdName, args) 

    
jsArgTupleInput :: (Fractional a, Read a, Stream s m Char) => ParsecT s u m [(Expr a, [Dependency])]
jsArgTupleInput = do
  char '(' *> sepBy jsExpression (char ',') <* char ')'
