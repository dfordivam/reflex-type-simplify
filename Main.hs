{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GADTs #-}

module Main where

import Language.Haskell.Exts
import Control.Monad

data ReflexAst where
  MonadicT :: ReflexAst -> ReflexAst
  DynamicT :: ReflexAst -> ReflexAst
  EventT :: ReflexAst -> ReflexAst
  MapT :: Show l => (Type l) -> ReflexAst -> ReflexAst
  TypeVar :: Show l => (Type l) -> ReflexAst

main = undefined

parseSourceType :: String -> Either String ReflexAst
parseSourceType s =
  case parseType s of
    f@(ParseFailed _ _) -> Left (show f)
    (ParseOk t@(TyApp _ _ _)) -> doParseType t
    _ -> Left "Not a valid type"

doParseType :: (Show l) => Type l -> Either String ReflexAst
doParseType (TyApp _ t1 t2) = makeAst t1 t2
doParseType (TyParen _ t) = doParseType t
-- doParseType (TyCon _ n) =
doParseType v@SomeType = Right (TypeVar v)
doParseType v@(TyVar _ n) = Right (TypeVar v)

doParseType _t = Left $ "Error parsing type:" ++ show _t

-- applyReflexAst :: ReflexAst -> ReflexAst -> ReflexAst
-- applyReflexAst (DynamicT )

pattern SomeType <-
  (TyCon _ (UnQual _ (Ident _ _)))
pattern Monadic <-
  (TyVar _ (Ident _ "m"))
pattern Dynamic <-
  (TyApp _ (TyCon _ (UnQual _ (Ident _ "Dynamic"))) (TyVar _ (Ident _ t)))
pattern Event <-
  (TyApp _ (TyCon _ (UnQual _ (Ident _ "Event"))) (TyVar _ (Ident _ t)))
pattern Map v <-
  (TyApp _ (TyCon _ (UnQual _ (Ident _ "Map"))) v)

makeAst :: (Show l) => Type l -> Type l -> Either String ReflexAst
makeAst Monadic t2 = MonadicT <$> doParseType t2
makeAst Dynamic t2 = DynamicT <$> doParseType t2
makeAst Event t2 = EventT <$> doParseType t2
makeAst (Map v) t2 = MapT v <$> doParseType t2
makeAst _t1 _t2 = Left $ "makeAst not yet implemented for:" ++ show _t1 ++ show _t2

instance Show ReflexAst where
  show (MonadicT ast) = "m (" ++ show ast ++ ")"
  show (DynamicT ast) = "Dynamic t (" ++ show ast ++ ")"
  show (EventT ast) = "Event t (" ++ show ast ++ ")"
  show (MapT k ast) = "Map (" ++ prettyPrint k ++ ") (" ++ show ast ++ ")"
  show (TypeVar v) = prettyPrint v
