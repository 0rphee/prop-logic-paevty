{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Foldable (for_)
import Data.Text (Text)
import Data.Text qualified as T
import Lib
import Test.Tasty
import Test.Tasty.HUnit
import Text.Megaparsec
import Text.Megaparsec.Debug (dbg)

main :: IO ()
main = do
  debugMain
  putStrLn "\n"
  defaultMain $ testMainGroups

testMainGroups :: TestTree
testMainGroups =
  testGroup "tests" $
    [ parsingTests,
      evaluationTests
    ]

evaluationTestCase :: Text -> [(Expr, Bool)] -> Bool -> TestTree
evaluationTestCase prop simpleVals desiredResult =
  let strProp = T.unpack prop
      right = do
        res <- case parseAndEval prop simpleVals of
          Just res -> pure res
          Nothing -> assertFailure "parser of evaluation error"
        desiredResult @?= res
        pure ()
   in testCase strProp right

evaluationTests :: TestTree
evaluationTests =
  testGroup
    "Evaluation Tests"
    [ evaluationTestCase "P" [(Simple "P", True)] True,
      evaluationTestCase "(P and Q)" [(Simple "P", True), (Simple "Q", True)] True,
      evaluationTestCase "(P and not Q)" [(Simple "P", True), (Simple "Q", True)] False,
      evaluationTestCase "((P or R) and not Q)" [(Simple "P", True), (Simple "Q", True), (Simple "R", False)] False,
      evaluationTestCase "(P and not (Q then R))" [(Simple "P", True), (Simple "Q", True), (Simple "R", False)] True

    ]

parsingTestCase :: Text -> Parser Expr -> Expr -> TestTree
parsingTestCase prop parser desiredResult =
  let strProp = T.unpack prop
      right = do
        res <- case runParser parser strProp prop of
          Right expr -> pure expr
          Left _ -> assertFailure "parser error"
        desiredResult @?= res
        pure ()
   in testCase strProp right

parsingTests :: TestTree
parsingTests =
  testGroup
    "Parsing Tests"
    [ parsingTestCase "p" parseSimple (Simple "p"),
      parsingTestCase "not p" parseNot $ Not (Simple "p"),
      parsingTestCase "(p and q)" parseAnd $ Simple "p" `And` Simple "q",
      parsingTestCase "(p or q)" parseOr $ Simple "p" `Or` Simple "q",
      parsingTestCase "(p then q)" parseCond $ Simple "p" `Cond` Simple "q",
      parsingTestCase "(p bithen q)" parseBicond $ Simple "p" `BiCond` Simple "q",
      parsingTestCase "not (p and q)" parseExpr $ Not (Simple "p" `And` Simple "q"),
      parsingTestCase
        "((not (p and q)) then ((r or s) bithen not k))"
        parseExpr
        ((Not (Simple "p" `And` Simple "q")) `Cond` ((Simple "r" `Or` Simple "s") `BiCond` (Not (Simple "k"))))
    ]

debugMain :: IO ()
debugMain =
  for_
    [ "p",
      "not p",
      "(p and q)",
      "(p or q)",
      "(p then q)",
      "(p bithen q)",
      "not (p and q)",
      "((not (p and q)) then ((p or q) bithen not p))"
    ]
    (`newDebugTestCase` dbg "expr" parseExpr)

newDebugTestCase :: Text -> Parser Expr -> IO ()
newDebugTestCase prop parser = do
  putStrLn $ "\n checking: " <> strProp
  print $ runParser parser strProp prop
  where
    strProp = T.unpack prop
