{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}

module Lib (runApp, Expr (..), Parser, parseExpr, parseAnd, parseBicond, parseAndEval, parseCond, parseNot, parseOr, parseSimple) where

import Control.Applicative
import Control.Monad (when)
import Data.Char (isAlpha)
import Data.Foldable (Foldable (foldMap', foldl'))
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

-- ======================== EVALUATION ========================

parseAndEval :: Text -> [(Expr, Bool)] -> Maybe Bool
parseAndEval text simpleValues = case runParser parseExpr "" text of
  Left _ -> Nothing
  Right expr -> eval expr simpleValues

eval :: Expr -> [(Expr, Bool)] -> Maybe Bool
eval expr vals =
  case expr of
    s@(Simple !_) -> lookup s vals
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

getSimExprs :: Expr -> Set Expr
getSimExprs = go S.empty
  where
    go accum expr = case expr of
      s@(Simple _) -> S.insert s accum
      Not e -> getSimExprs e
      And le re -> bi le re
      Or le re -> bi le re
      Cond le re -> bi le re
      BiCond le re -> bi le re
      where
        bi l r = getSimExprs l <> getSimExprs r

genColAndHeaders :: Set Expr -> (Text, Text)
genColAndHeaders =
  S.foldl'
    ( \(cAccum, hAccum) next ->
        let uNext = case next of
              Simple a -> a
              _ -> error "this can never happen"
            cuAccum =
              if T.null cAccum
                then cAccum
                else cAccum <> ", "
         in ([i|#{cuAccum}auto|], [i|#{hAccum}[*$#{uNext}$*], |])
    )
    ("", "")

appendFinalProp :: Expr -> [[(Expr, Bool)]] -> Maybe [[(Expr, Bool)]]
appendFinalProp expr ls =
  case for ls $ \a -> eval expr a of
    Nothing -> Nothing
    Just evalResults -> Just $ zipWith (\b list -> list <> [(Simple "_", b)]) evalResults ls

genPosibilities :: Set a -> [[(a, Bool)]]
genPosibilities set =
  let a = S.toList set
      b = fmap (\v -> [(v, True), (v, False)]) a
      c = sequence b
   in c

posibilitiesToText :: Int -> [[(Expr, Bool)]] -> Text
posibilitiesToText indent ls = T.unlines $ fmap (foldl' foldFunc (T.replicate indent " ")) ls
  where
    foldFunc :: Text -> (Expr, Bool) -> Text
    foldFunc accum (expr, next) =
      let uNextChar = if next then 'V' else 'F'
          uNext :: Text = case expr of
            Simple "_" -> [i|align(center)[$#{uNextChar}$]|]
            _ -> [i|$#{uNextChar}$|]
       in [i|#{accum}#{uNext}, |]

-- TODO: print subexpressins

runApp :: Text -> Bool -> Either (ParseErrorBundle Text Void) Text
runApp entryStr printSubexpr = case runParser topLevelparseExpr "" entryStr of
  Left e -> Left e
  Right expr -> pure $ foldMap' makeSingleTable expr
  where
    makeSingleTable :: Expr -> Text
    makeSingleTable singleExpr =
      let simpleExpressions = getSimExprs singleExpr
          (cols, headers) = genColAndHeaders simpleExpressions
          posibilities = genPosibilities simpleExpressions
          posibilitiesWithResults = case appendFinalProp singleExpr posibilities of
            Nothing -> error "this shouldn't happen"
            Just a -> a
          textPosibilites = posibilitiesToText 2 posibilitiesWithResults
       in [i|
  \#let then = $arrow$
  \#let bithen = $arrow.l.r$

  \#table(
    columns: (#{cols}, auto),
    inset: 10pt,
    align: horizon,
    #{headers} [*$#{prettyExpr singleExpr}$*],

  #{textPosibilites}
  )
  |]
