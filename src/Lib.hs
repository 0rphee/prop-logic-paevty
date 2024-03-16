{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}

module Lib (runApp, Expr (..), Parser, parseExpr, parseAnd, parseBicond, parseAndEval, parseCond, parseNot, parseOr, parseSimple) where

import Control.Applicative
import Control.Monad (when)
import Data.Char (isAlpha)
import Data.Foldable
import Data.Map.Strict qualified as M
import Data.Set (Set)
import Data.Set qualified as S
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Traversable (for)
import Data.Void (Void)
import Text.Megaparsec (MonadParsec (takeWhile1P, try), ParseErrorBundle, Parsec, choice, chunk, runParser)
import Text.Megaparsec.Char (char, space)

-- ======================== PARSING ========================

type Parser a = Parsec Void Text a

topLevelparseExpr :: Parser [Expr]
topLevelparseExpr = do
  many
    ( do
        expr <- parseExpr
        (space >> char ';' >> space) <|> space
        pure expr
    )

parseExpr :: Parser Expr
parseExpr =
  choice
    [ parseSimple,
      parseNot,
      parseAnd,
      parseOr,
      parseCond,
      parseBicond
    ]

parseSimple :: Parser Expr
parseSimple = try $ do
  space
  a <- takeWhile1P Nothing isAlpha
  let identifierCheck = \case
        "and" -> True
        "or" -> True
        "then" -> True
        "bithen" -> True
        "not" -> True
        _ -> False
  when (identifierCheck a) $ fail ("found '" <> T.unpack a <> "' as an identifier")

  pure (Simple a)

parseNot :: Parser Expr
parseNot = do
  space
  choice
    [ try $ do
        _ <- char '('
        r <- inner
        _ <- char ')'
        pure r,
      inner
    ]
  where
    inner = chunk "not" >> space >> Not <$> parseExpr

parseBinary :: Text -> (Expr -> Expr -> Expr) -> Parser Expr
parseBinary name constructor = try $ do
  space >> char '(' >> space
  left <- parseExpr
  space >> chunk name >> space
  right <- parseExpr
  _ <- space >> char ')'
  pure $ left `constructor` right

parseAnd :: Parser Expr
parseAnd = parseBinary "and" And

parseOr :: Parser Expr
parseOr = parseBinary "or" Or

parseCond :: Parser Expr
parseCond = parseBinary "then" Cond

parseBicond :: Parser Expr
parseBicond = parseBinary "bithen" BiCond

-- faill :: Parser Expr
-- faill = do
--   a <- lookAhead anySingle
--   failure (Just . Tokens $ N.singleton a) S.empty

data Expr
  = Simple !Text
  | Not !Expr
  | And !Expr !Expr
  | Or !Expr !Expr
  | Cond !Expr !Expr
  | BiCond !Expr !Expr
  deriving (Show, Eq, Ord)

newtype SimExpr = SimExpr {unSimExpr :: Expr}
  deriving newtype (Show, Eq, Ord)

-- ======================== EVALUATION ========================

parseAndEval :: Text -> M.Map SimExpr Bool -> Maybe Bool
parseAndEval text simpleValues = case runParser parseExpr "" text of
  Left _ -> Nothing
  Right expr -> eval expr simpleValues

eval :: Expr -> M.Map SimExpr Bool -> Maybe Bool
eval expr vals =
  case expr of
    s@(Simple !_) -> M.lookup (SimExpr s) vals
    Not !e -> not <$> eval e vals
    And !le !re -> bi (&&) le re
    Or !le !re -> bi (||) le re
    Cond !le !re -> bi cond le re
    BiCond !le !re -> bi bicond le re
  where
    bi op l r = liftA2 op (eval l vals) (eval r vals)
    cond l r = case (l, r) of
      (True, False) -> False
      _ -> True
    bicond l r = case (l, r) of
      (True, True) -> True
      (False, False) -> True
      _ -> False

-- ======================== PRINTING ========================

prettyExpr :: Expr -> Text
prettyExpr = \case
  Simple a -> a
  Not a -> [i|not #{prettyExpr a}|]
  And a b -> [i|(#{prettyExpr a} and #{prettyExpr b})|]
  Or a b -> [i|(#{prettyExpr a} or #{prettyExpr b})|]
  Cond a b -> [i|(#{prettyExpr a} then #{prettyExpr b})|]
  BiCond a b -> [i|(#{prettyExpr a} bithen #{prettyExpr b})|]

getSimExprs :: Expr -> Set SimExpr
getSimExprs = go S.empty
  where
    go accum expr = case expr of
      s@(Simple _) -> S.insert (SimExpr s) accum
      Not e -> getSimExprs e
      And le re -> bi le re
      Or le re -> bi le re
      Cond le re -> bi le re
      BiCond le re -> bi le re
      where
        bi l r = getSimExprs l <> getSimExprs r

-- | an interpretation is a Map of the assignments of the truth values of simple expressions
appendFinalProp ::
  -- | the complexExprs are the expressions to be evaluated by a given interpretation Map SimExpr Bool of simple values. (ex. a and b)
  [Expr] ->
  -- |  a and b -> interpretation 1: a=T b=F, etc. :: Map SimExpr Bool
  [M.Map SimExpr Bool] ->
  -- | the output is the Maybe of a list of results of a given interpretation. Each element of the list is a an intepretation (a line in the table output)
  Maybe [[(SimExpr, Bool)]]
appendFinalProp complexExpressions listOfIntepretations =
  case for listOfIntepretations $ \interp ->
    for complexExpressions $ \complexExpr ->
      (SimExpr . Simple $ prettyExpr complexExpr,) <$> eval complexExpr interp of
    -- for every interpretation of simple exprs (:: Map) (ex. p=T, q=F, etc.), eval that expression
    Nothing -> Nothing
    Just evalResults ->
      let inMap = evalResults
       in Just $ zipWith (<>) (M.toList <$> listOfIntepretations) inMap

genPosibilities :: (Ord a) => Set a -> [M.Map a Bool]
genPosibilities set =
  let a = S.toList set
      b = fmap (\v -> [(v, True), (v, False)]) a
      c = sequence b
   in fmap M.fromList c

posibilitiesToText :: Int -> [[(SimExpr, Bool)]] -> Text
posibilitiesToText indent ls = T.unlines $ fmap (foldl' foldFunc (T.replicate indent " ")) ls
  where
    foldFunc :: Text -> (SimExpr, Bool) -> Text
    foldFunc accum (expr, next) =
      let uNextChar = if next then 'V' else 'F'
          uNext :: Text = case expr of
            SimExpr (Simple t) | T.any (== '(') t -> [i|align(center)[$#{uNextChar}$]|]
            _ -> [i|$#{uNextChar}$|]
       in [i|#{accum}#{uNext}, |]

-- TODO: print subexpressins

runApp :: Text -> Bool -> Bool -> Bool -> Either (ParseErrorBundle Text Void) Text
runApp entryStr printSubexpr mergeTables useGradientStroke = case runParser topLevelparseExpr "" entryStr of
  Left e -> Left e
  Right exprs ->
    -- each expression in `exprs`, is one formula parsed between ';'. "(a and b) ; (p or q)" ->
    pure $
      let simExprsSet = getSimpleExprs exprs
          tables =
            if mergeTables
              then merge simExprsSet
              else notMerge simExprsSet
       in [i|
  \#let then = $arrow$
  \#let bithen = $arrow.l.r$

#{tables}
|]
  where
    merge :: [(Expr, S.Set SimExpr)] -> Text
    merge = M.foldMapWithKey makeOneTable . getSimpleExprsMap

    notMerge :: [(Expr, S.Set SimExpr)] -> Text
    notMerge = foldMap (\(expr, simExprs) -> makeOneTable simExprs (S.singleton expr))

    getSimpleExprs :: [Expr] -> [(Expr, S.Set SimExpr)]
    getSimpleExprs = fmap (\e -> (e, getSimExprs e))

    getSimpleExprsMap :: [(Expr, S.Set SimExpr)] -> M.Map (S.Set SimExpr) (S.Set Expr)
    getSimpleExprsMap a = go a M.empty
      where
        go :: [(Expr, S.Set SimExpr)] -> M.Map (S.Set SimExpr) (S.Set Expr) -> M.Map (S.Set SimExpr) (S.Set Expr)
        go [] accum = accum
        go ((expr, simexprs) : xs) accum =
          let f :: Maybe (S.Set Expr) -> Maybe (S.Set Expr)
              f = \case
                Nothing -> Just $ S.singleton expr
                Just ys -> Just $ S.insert expr ys
           in go xs (M.alter f simexprs accum)

    makeOneTable :: S.Set SimExpr -> S.Set Expr -> Text
    makeOneTable simpleExprs complexExprs =
      let prettyHeader expr = [i|[*$#{prettyExpr expr}$*]|] :: Text
          gradient =
            if useGradientStroke
              then "    stroke: gradient.linear(red, blue),"
              else "" :: Text
          headers = T.intercalate ", " $ (prettyHeader . unSimExpr <$> S.toList simpleExprs) <> (prettyHeader <$> S.toList complexExprs)
          numOfColumns = S.size simpleExprs + S.size complexExprs
          posibilities = genPosibilities simpleExprs
          posibilitiesWithResults = case appendFinalProp (S.toList complexExprs) posibilities of
            Nothing -> error "this shouldn't happen"
            Just a -> a
          textPosibilites = posibilitiesToText 4 posibilitiesWithResults
       in [i|
  \#table(
    columns: #{numOfColumns},
    inset: 10pt,
    align: horizon,
#{gradient}
    #{headers},
#{textPosibilites}
  )

|]
