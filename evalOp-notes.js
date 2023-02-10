  case varDecl of
    Nothing -> pure () -- the evaluation of expression may have updated an Object tho
    --------------------Can only affect innermost---------------------------------------
    -- is like Data.Map.insert
    -- NOTE! All values should be a PureOp or Class or Function
    -- NOTE! classes may be overwritten by values 
    Just (Raw A_Equal ref') -> 
      -- | Can only affect the innermost function AST (if we are in a fn that is)
      case mMyOwnAST of
        Just myAst -> putSubStateT myAst name objectOriented
        Nothing -> do
          ast <- getAST
          -- | TODO(galen): Can this handle setting a new property ? this.x = 1? 
          putAST ast name objectOriented          
    -- Is like Data.Map.adjust :: (a -> a) -> k -> Map k a
    -- | TODO(galen): should disallow change for const (and A_Equal for let)
    -- | TODO(galen): should this use evalRef? 
    Just (Raw iterOp ref') -> do 
      -- | Can update and change global or transitory declarations 
      oo <- getsAST ref' -- TODO(galen): lens for maps
      -- | Note that the next line works regardless of if its actually a Value or Expr
      -- | Although we may need to make the '[Function: name]' and '[class name]' strings  
      putAST $ PureOp $ Op (toAssocOperator iterOp) oo objectOriented
    -- is like Data.Map.insert
    -- TODO: should disallow change for prev Const and Let 
    Just notRawName ->
      -- | TODO(galen): it would be valuable to check if Const was used to define this variable we intend to implement
      -- | SUB-TODO: we'd need to handle an error here: what should the business logic be for errors? Configurable?
      let toName = \case
            Let n -> n 
            Var n -> n
            Const n -> n
            _ -> error "wtf" 
      in case mMyOwnAST of
           Just myAst -> putSubStateT myAst name objectOriented 
           Nothing -> do
             ast <- getAST 
             putAST ast name objectOriented
    --------------------Can only affect innermost---------------------------------------      

