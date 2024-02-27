{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Lib (runApp)
import Options.Applicative
import Options.Applicative.Help.Pretty

main :: IO ()
main = do 
  (NormalOptions prop printSubexpr outputPth) <- execParser options
  let res = runApp prop printSubexpr
  TIO.writeFile outputPth res


data Options
  = NormalOptions
      !T.Text --  proposition
      !Bool -- don't print subexpressions
      !FilePath -- outputFilePath

options :: ParserInfo Options
options =
  info
    (opts <**> helper)
    ( fullDesc
        <> header
          "schedule-maker - a command-line utility to create your school schedules."
        <> footer
          "still a work in progress, source code here: https://github.com/0rphee/schedule-maker"
        <> progDesc
          "Create an .xlsx file with your school schedules from a .yaml file with your classes."
        <> failureCode 64
    )

opts :: Parser Options
opts = NormalOptions <$> proposition <*> prettyPrintStdout <*> outputPath

proposition :: Parser T.Text
proposition = strArgument (metavar "EXPRESSSION" <> helpDoc (Just doc))
  where doc = "The expression to be evaluated, in the form:" <> line <> line
            <> "<formula> ::= <simple> | <negation> <formula> | <binary-formula>" <> line
            <> "<simple> ::= a | z | ... | A | ... | Z" <> line
            <> "<negation> ::= not | ¬ | ~" <> line
            <> "<binary-formula> ::= <conjunction> | <disyunction> | <implication> | <bi-implication" <> line
            <> "<conjunction> ::= (<formula> and <formula>) | (<formula> & <formula>) | (<formula> ∧ <formula>)" <> line
            <> "<disyunction> ::= (<formula> or <formula>) | (<formula> ∨ <formula>)" <> line
            <> "<implication> ::= (<formula> -> <formula>) | (<formula> then <formula>) | (<formula> → <formula>)" <> line
            <> "<bi-implication> ::= (<formula> <-> <formula>) | (<formula> bithen <formula>)"



prettyPrintStdout :: Parser Bool
prettyPrintStdout =
  switch
    ( help "Print to stdout the validated schedules"
        <> short 'p'
        <> long "pretty-print"
    )

outputPath :: Parser FilePath
outputPath =
  strOption
    ( metavar "FILENAME"
        <> help "Write output to FILE (.typst)"
        <> action "directory"
        <> action "file"
        <> showDefault
        <> value "out.typst"
        <> long "output"
        <> short 'o'
    )
