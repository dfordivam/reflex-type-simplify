{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Language.Haskell.Exts
import Control.Monad
import Data.Tree
import Data.Foldable
import Data.Sequence
import Data.Maybe

import           Hedgehog
import           Hedgehog.Internal.Property (TestLimit(..))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE

data ReflexAst where
  MonadicT :: ReflexAst -> ReflexAst
  DynamicT :: ReflexAst -> ReflexAst
  EventT :: ReflexAst -> ReflexAst
  MapT :: Show l => (Type l) -> ReflexAst -> ReflexAst
  TypeVar :: Show l => (Type l) -> ReflexAst

data Operation
  -- Pure Operations
  = Updated -- Dynamic a -> Event a
  | TagPromptlyDyn -- Dynamic a -> Event b -> Event a
  | HoldDyn -- a -> Event a -> m (Dynamic a)
  | FoldDyn -- (a -> b -> b ) -> b -> Event a -> m (Dynamic b)
  | Join -- Dynamic (Dynamic a) ->  Dynamic a
  | JoinDynThroughMap -- Dynamic (Map k (Dynamic a)) ->  Dynamic (Map k a)
  | SwitchPromptlyDyn -- Dynamic (Event a)  ->    Event a
  | Coincidence -- Event (Event a)  ->    Event a
  | SwitchPromptly -- Event a -> Event (Event a)  -> m (Event a)
  | MergeMap -- Map k (Event a) -> Event (Map k a)
  | ReduceMap -- Map k (Event a) -> Event a (Use mergeList.toList)
  -- Monadic operations
  | Dyn -- Dynamic (m a) -> m (Event a)
  | WidgetHold -- m a ->   Event (m a) -> m (Dynamic a)
  | ListWithKey -- Dynamic (Map k v) -> (k -> Dynamic v -> m a ) -> m (Dynamic (Map k a))
  | SimpleList -- Dynamic [v] -> (     Dynamic v -> m a ) -> m (Dynamic [a])
  | ListWithKey' -- Map k v -> Event (Map k (Maybe v)) -> (k -> v -> Event v -> m a) -> m (Dynamic (Map k a))
  | PerformEvent -- Event (WidgetHost m  a) -> m (Event a)
  deriving (Show, Enum, Bounded)

data OperationTree
  = Operations (NonEmpty Operation)
  | FMap OperationTree
  deriving (Show)

main =
  checkParallel $
  Group
    "testing"
    [ ("test", testStr "Dynamic t (Dynamic t (Event t Int))" "Event t Int")
    , ( "test2"
      , testStr "Dynamic t (Dynamic t (Event t Int))" "m (Dynamic t Int)")
    , ( "test3"
      , testStr
          "Dynamic t (Dynamic t (Dynamic t (Event t Int)))"
          "m (Dynamic t Int)")
    , ( "test4"
      , testStr
          "Dynamic t (Map k (Event t Int))"
          "m (Event t (Map k Int))")
    , ( "test5"
      , testStr
          "Event t (Map k (Event t Int))"
          "Event t (Map k Int)")
    , ("test6", testStr "Dynamic t (Dynamic t (Event t Int))"
        "m (Event t (Event t Int))")
    ]

-- Simplification
simplify :: ReflexAst -> Tree (Operation, ReflexAst)
simplify = undefined

testStr :: String -> String -> Property
testStr str1 str2 = withTests (TestLimit 1000000) . property $ do
  let
      -- (Right init) = parseSourceType "m (Dynamic t (m (Event t Int)))"
      -- (Right res) = parseSourceType "m (Event t Int)"
      (Right init) = parseSourceType str1
      (Right res) = parseSourceType str2

  --s <- forAll $ Gen.small (ops init)
  s <- forAll $ (opsManualRecurse init)
  let
    ret = applyOpTree init s
  assert $ ret /= (Just res)

opGen :: Monad m => Gen m Operation
opGen = Gen.enumBounded

opTreeGen :: Monad m => Gen m OperationTree
opTreeGen = Gen.recursive Gen.choice
  [Operations <$> Gen.nonEmpty (Range.linear 0 5) opGen]
  [FMap <$> opTreeGen]

-- ops :: Monad m => ReflexAst -> Gen m ([Operation])
-- ops ast =
--   Gen.filter (\ops -> isJust $ applyOps ast ops)
--     (Gen.list (Range.linear 0 5) opGen)

opsManualRecurse :: Monad m => ReflexAst -> Gen m [OperationTree]
opsManualRecurse ast = do
  let
    initSafe [] = []
    initSafe xs = init xs
    l os = do
       o <- opTreeGen
       let os' = os ++ [o]
       case (applyOpTree ast os') of
         Nothing ->
           -- case (applyOps ast osFmap) of
           --   Nothing ->
               Gen.frequency [(1, l (initSafe os)), (20, l os)]
             -- Just _ ->
             --   Gen.recursive Gen.choice [return osFmap] [l osFmap]
           -- Gen.recursive Gen.choice [] [l [], l (initSafe os)]
         Just _ ->
           Gen.frequency [(1, return os'), (4, l os')]
           -- Gen.recursive Gen.choice [return os'] [l os']
  l []


-- Operations
op1 :: ReflexAst -> Operation -> Maybe ReflexAst
op1 (DynamicT a) Updated = Just $ EventT a
op1 (EventT a) HoldDyn = Just $ MonadicT (DynamicT a)
op1 (DynamicT (DynamicT a)) Join = Just $ DynamicT a
op1 (MonadicT (MonadicT a)) Join = Just $ MonadicT a
op1 (DynamicT (MapT v (DynamicT a))) JoinDynThroughMap = Just $ DynamicT (MapT v a)
op1 (DynamicT (EventT a)) SwitchPromptlyDyn = Just $ EventT a
op1 (EventT (EventT a)) SwitchPromptly = Just $ MonadicT (EventT a)
op1 (DynamicT (MonadicT a)) Dyn = Just $ MonadicT (EventT a)
op1 (EventT (MonadicT a)) WidgetHold = Just $ MonadicT (DynamicT a)
op1 (DynamicT (MapT k a)) ListWithKey = Just $ MonadicT (DynamicT (MapT k a))
op1 (MapT k (EventT a)) MergeMap = Just $ EventT (MapT k a)
op1 (MapT k (EventT a)) ReduceMap = Just $ EventT a
op1 _ _ = Nothing

opFMap :: ReflexAst -> OperationTree -> Maybe ReflexAst
opFMap (MonadicT a) ops = MonadicT <$> applyOps a ops
opFMap (DynamicT a) ops = DynamicT <$> applyOps a ops
opFMap (EventT a)   ops =   EventT <$> applyOps a ops
opFMap _ _ = Nothing


applyOps :: ReflexAst -> OperationTree -> Maybe ReflexAst
applyOps ast (FMap opTree) = opFMap ast opTree
applyOps ast (Operations ops) = foldM op1 ast ops

applyOpTree :: ReflexAst -> [OperationTree] -> Maybe ReflexAst
applyOpTree = foldM applyOps

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

instance Eq ReflexAst where
  (MonadicT ast1) == (MonadicT ast2) = ast1 == ast2
  (DynamicT ast1) == (DynamicT ast2) = ast1 == ast2
  (EventT ast1) == (EventT ast2) = ast1 == ast2
  (MapT t1 ast1) == (MapT t2 ast2) = (prettyPrint t1 == prettyPrint t2) && ast1 == ast2
  (TypeVar t1) == (TypeVar t2) = (prettyPrint t1 == prettyPrint t2)
  (==) _ _ = False
