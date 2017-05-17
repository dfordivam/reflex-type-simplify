{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GADTs #-}

module Main where

import Language.Haskell.Exts
import Control.Monad
import Data.Tree

data ReflexAst where
  MonadicT :: ReflexAst -> ReflexAst
  DynamicT :: ReflexAst -> ReflexAst
  EventT :: ReflexAst -> ReflexAst
  MapT :: Show l => (Type l) -> ReflexAst -> ReflexAst
  TypeVar :: Show l => (Type l) -> ReflexAst

data Operation
  -- Pure Operations
  = FMap -- Dynamic a -> Dynamic b
  | Updated -- Dynamic a -> Event a
  | TagPromptlyDyn -- Dynamic a -> Event b -> Event a
  | HoldDyn -- a -> Event a -> m (Dynamic a)
  | FoldDyn -- (a -> b -> b ) -> b -> Event a -> m (Dynamic b)
  | Join -- Dynamic (Dynamic a) ->  Dynamic a
  | JoinDynThroughMap -- Dynamic (Map k (Dynamic a)) ->  Dynamic (Map k a)
  | SwitchPromptlyDyn -- Dynamic (Event a)  ->    Event a
  | Coincidence -- Event (Event a)  ->    Event a
  | SwitchePromptly -- Event a -> Event (Event a)  -> m (Event a)

  -- Monadic operations
  | Dyn -- Dynamic (m a) -> m (Event a)
  | WidgetHold -- m a ->   Event (m a) -> m (Dynamic a)
  | ListWithKey -- Dynamic (Map k v) -> (k -> Dynamic v -> m a ) -> m (Dynamic (Map k a))
  | SimpleList -- Dynamic [v] -> (     Dynamic v -> m a ) -> m (Dynamic [a])
  | ListWithKey' -- Map k v -> Event (Map k (Maybe v)) -> (k -> v -> Event v -> m a) -> m (Dynamic (Map k a))
  | PerformEvent -- Event (WidgetHost m  a) -> m (Event a)

main = undefined

-- Simplification
simplify :: ReflexAst -> Tree (Operation, ReflexAst)
simplify = undefined

-- Parsing
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
