module Snarkl.CLI.Compile
  ( OptimiseOpts (..),
    CompileOpts (..),
    compileOptsParser,
  )
where

import Options.Applicative

data OptimiseOpts = OptimiseOpts
  { simplify :: Bool,
    removeUnreachable :: Bool
  }

optimizeOptsParser :: Parser OptimiseOpts
optimizeOptsParser =
  OptimiseOpts
    <$> switch
      ( long "simplify"
          <> help "run the constraint simplifier"
      )
    <*> switch
      ( long "remove-unreachable"
          <> help "detect and remove variables not contributing to the output"
      )

data CompileOpts = CompileOpts
  { optimiseOpts :: OptimiseOpts,
    r1csOutput :: FilePath
  }

compileOptsParser :: Parser CompileOpts
compileOptsParser =
  CompileOpts
    <$> optimizeOptsParser
    <*> strOption
      ( long "r1cs-output-dir"
          <> help "the directory to write the r1cs artifact"
      )
