{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GADTs #-}

module Main where

import Language.Haskell.Exts
import Control.Monad

data ReflexAst where
  DynamicT :: ReflexAst -> ReflexAst
  EventT :: ReflexAst -> ReflexAst
  MapT :: Show l => (Type l) -> ReflexAst -> ReflexAst
  TypeVar :: Show l => (Type l) -> ReflexAst

instance Show ReflexAst where
  show (DynamicT ast) = "Dynamic t (" ++ show ast ++ ")"
  show (EventT ast) = "Event t (" ++ show ast ++ ")"
  show (MapT k ast) = "Map (" ++ prettyPrint k ++ ") (" ++ show ast ++ ")"
  show (TypeVar v) = prettyPrint v

main = undefined

parseSourceType :: String -> Either String ReflexAst
parseSourceType s =
  case parseType s of
    f@(ParseFailed _ _) -> Left (show f)
    (ParseOk t@(TyApp _ _ _)) -> doParseType t
    _ -> Left "Not a valid type"

doParseType :: (Show l) => Type l -> Either String ReflexAst
doParseType (TyApp _ t1 t2) = makeAst t1 t2
-- doParseType (TyCon _ n) =
doParseType v@(TyVar _ n) = Right (TypeVar v)

doParseType _ = Left "Error parsing type"

-- applyReflexAst :: ReflexAst -> ReflexAst -> ReflexAst
-- applyReflexAst (DynamicT )

pattern Dynamic <-
  (TyApp _ (TyCon _ (UnQual _ (Ident _ "Dynamic"))) (TyVar _ (Ident _ t)))

makeAst :: (Show l) => Type l -> Type l -> Either String ReflexAst
makeAst Dynamic t2 = DynamicT <$> doParseType t2
