{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}

module Lib where

import Data.Foldable
import Data.Set (Set)
import Data.Set qualified as S
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Traversable (for)
import Data.Void (Void)
import Text.Megaparsec (MonadParsec (notFollowedBy, try), Parsec, choice, chunk, errorBundlePretty, runParser)
import Text.Megaparsec.Char (char, letterChar, space)

-- ======================== PARSING ========================

type Parser a = Parsec Void Text a

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
  a <- Simple <$> letterChar
  notFollowedBy letterChar
  pure a

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
  = Simple !Char
  | Not !Expr
  | And !Expr !Expr
  | Or !Expr !Expr
  | Cond !Expr !Expr
  | BiCond !Expr !Expr
  deriving (Show, Eq, Ord)

-- data TExpr = TSi | TNSim

-- data GExpr (a :: TExpr) where
--   GSimple :: Char -> GExpr TSi
--   GNot :: GExpr a -> GExpr TNSim
--   GAnd :: GExpr a -> GExpr a -> GExpr TNSim
--   GOr :: GExpr a -> GExpr a -> GExpr TNSim
--   GCond :: GExpr a -> GExpr a -> GExpr TNSim
--   GBiCond :: GExpr a -> GExpr a -> GExpr TNSim

-- data KExpr = KSExpr !(GExpr TSi) | KNExpr !(GExpr TNSim)

-- a = GNot $ GSimple 'a'
-- b = GNot $ GNot $ GSimple 'a'

-- c = GNot
-- d = c $ GSimple 'b'
-- f = c $ d

-- exprToGExpr :: Expr -> GExpr a
-- exprToGExpr = \case
--   Simple e  -> Left $ GSimple e
--   Not e -> Right $ GNot $ exprToGExpr e
--   And l r -> Right $  bi GAnd l r
--   Or l r -> Right $  bi GOr l r
--   Cond l r -> Right $  bi GCond l r
--   BiCond l r -> Right $  bi GBiCond l r
--   where bi cons l r = cons (exprToGExpr l) (exprToGExpr r)

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
    Just evalResults -> Just $ zipWith (\b list -> list <> [(Simple '_', b)]) evalResults ls

genPosibilities :: Set a -> [[(a, Bool)]]
genPosibilities set =
  let a = S.toList set
      b = fmap (\v -> [(v, True), (v, False)]) a
      c = sequence b
   in c

posibilitiesToText :: Int -> [[(Expr, Bool)]] -> Text
posibilitiesToText indent ls = T.unlines $ fmap (foldl' foldFunc (T.replicate indent " ")) ls
  where
    foldFunc accum (expr, next) =
      let uNextChar = if next then 'V' else 'F'
          uNext = case expr of
            Simple '_' -> [i|align(center)[$#{uNextChar}$]|] :: Text
            _ -> [i|$#{uNextChar}$|]
       in [i|#{accum}#{uNext}, |] :: Text

runApp :: Text -> Bool -> Text
runApp entryStr printSubexpr = case runParser parseExpr "" entryStr of
  Left e -> T.pack $ errorBundlePretty e
  Right expr ->
    let simpleExprs = getSimExprs expr
        (cols, headers) = genColAndHeaders simpleExprs
        posibilities = genPosibilities simpleExprs
        posibilitiesWithResults = case appendFinalProp expr posibilities of
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
  #{headers} [*$#{entryStr}$*],

#{textPosibilites}
)

  |]
