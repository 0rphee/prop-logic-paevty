{-# LANGUAGE OverloadedStrings #-}

module CmdLineOpts (Options (..), options) where

import Data.Text qualified as T
import Options.Applicative
import Options.Applicative.Help.Pretty

data Options
  = NormalOptions
      -- | The propositions to be evaluated
      !T.Text
      -- |  True = print subexpressions
      !Bool
      -- |  True = print to stdout
      !Bool
      -- | outputFilePath
      !FilePath
      -- | True = merge proposition tables that share simple values
      !Bool
      -- | True = use 'gradient.linear(red, blue)' as the stroke color for the tables
      !Bool

options :: ParserInfo Options
options =
  info
    (opts <**> helper)
    ( fullDesc
        <> header
          "prop-logic-paevty - a command-line utility to parse formulas from propositional calculus and create its corresponding truth table into .typst format."
        <> footer
          "Still a work in progress, source code here: https://github.com/0rphee/prop-logic-paevty"
        <> progDesc
          "Create typst truth tables propositional calculus' formulas."
        <> failureCode 64
    )
  where
    opts :: Parser Options
    opts = NormalOptions <$> propositionText <*> printSubexpressions <*> prettyPrintStdout <*> outputFilePath <*> mergeTables <*> gradientColor

propositionText :: Parser T.Text
propositionText = strArgument (metavar "EXPRESSSION" <> helpDoc (Just doc))
  where
    doc =
      "The expression to be evaluated, in the form:"
        <> line
        <> line
        <> vsep
          [ form "formula" ["<simple>", "<negation>", "<formula>", "<binary>"],
            form "simple" ["a", "...", "z", "A", "...", "Z"],
            form "negation" ["not", "¬", "~"],
            form "binary" ["<conjunction>", "<disyunction>", "<implication>", "<bi-implication>"],
            form "conjunction" ["(<formula> and <formula>)", "(<formula> & <formula>)", "(<formula> ∧ <formula>)"],
            form "disyunction" ["(<formula> or <formula>)", "(<formula> ∨ <formula>)"],
            form "implication" ["(<formula> -> <formula>)", "(<formula> then <formula>)", "(<formula> → <formula>)"],
            form "bi-implication" ["(<formula> <-> <formula>)", "(<formula> bithen <formula>)"]
          ]
    form name contents = angles name <+> "::=" <+> align (concatWith (\l r -> l <> softline <> "|" <> softline <> r) contents)

prettyPrintStdout :: Parser Bool
prettyPrintStdout =
  switch
    ( help "Print to stdout the validated schedules"
        <> short 's'
        <> long "stdout"
    )

printSubexpressions :: Parser Bool
printSubexpressions =
  switch
    ( help "Print the subexpressions in the truth table."
        <> short 'e'
        <> long "subexpressions"
    )

outputFilePath :: Parser FilePath
outputFilePath =
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

mergeTables :: Parser Bool
mergeTables =
  switch
    ( help "Merge tables that share the same simple formulas."
        <> short 'm'
        <> long "merge"
    )

gradientColor :: Parser Bool
gradientColor =
  switch
    ( help "Whether to use 'gradient.linear(red, blue)' as the stroke of the tables."
        <> short 'c'
        <> long "color"
    )
