-- | Create and run child processes and retrieve results from them.
--
-- For example:
--
-- >>> StdoutTrimmed stdout <- run $ cmd "echo" & addArgs ["Hello, World!"]
-- >>> print stdout
-- "Hello, World!"
--
-- == Outputs
--
-- `run` is polymorphic in its output, the output type just has to implement
-- 'Output'. So for example you can get the exit code of a process like this:
--
-- >>> run $ cmd "false" :: IO ExitCode
-- ExitFailure 1
--
-- If you don't want to retrieve any information from a child process, you can
-- use `run_` (or make the output type '()').
--
-- For more information on available output types, see 'Output'.
--
-- == Process Configuration
--
-- To modify the setup of the child process -- e.g. to add arguments or modify
-- stdin or stdout, etc. -- you can use one of the functions that modify
-- 'ProcessConfiguration.ProcessConfiguration', see [here](#processConfiguration). Here's how you add
-- arguments, for example:
--
-- >>> run_ $ cmd "echo" & addArgs ["foo", "bar"]
-- foo bar
-- >>> run_ $ cmd "echo"
-- <BLANKLINE>
--
-- == No Shell, No Automatic Splitting of Strings
--
-- `cradle` will never wrap your process in a shell process.
--
-- `cradle` will not split any inputs by whitespace. So e.g. this doesn't work:
--
-- >>> run_ $ cmd "echo foo bar"
-- *** Exception: echo foo bar: Cradle.run: posix_spawnp: does not exist (No such file or directory)
--
-- This is trying to run an executable with the file name @"echo foo"@, which
-- doesn't exist. If you want to split up arguments automatically, you can do
-- that in haskell though:
--
-- >>> run_ $ cmd "echo" & addArgs (words "foo bar")
-- foo bar
module Cradle
  ( -- * Running Child Processes
    run,
    run_,
    (&),
    -- | #processConfiguration#

    -- * Process Configuration

    -- | Configuration on how to run a process. You can
    --
    --   * create one with 'cmd',
    --
    --   * configure it with functions from 'Cradle.ProcessConfiguration.Helpers',
    --     (which are re-exported from here for convenience) and
    --
    --   * run the process with 'run' or 'run_'.
    --
    -- Usually it shouldn't be necessary to modify its fields directly, but you
    -- *can* import the constructors and fields from
    -- 'Control.ProcessConfiguration'.
    ProcessConfiguration,
    cmd,
    module Cradle.ProcessConfiguration.Helpers,

    -- * Possible Outputs
    Output,
    StdoutUntrimmed (..),
    StdoutTrimmed (..),
    StdoutRaw (..),
    StderrRaw (..),
    ExitCode (..),
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Cradle.Output
import Cradle.ProcessConfiguration
import Cradle.ProcessConfiguration.Helpers
import Data.Function ((&))
import System.Exit (ExitCode (..))

run :: (Output output, MonadIO m) => ProcessConfiguration -> m output
run config = do
  modifiers' <- liftIO $ modifiers config
  liftIO $ runAndGetOutput (modifiers' config)

-- | Same as `run`, but always returns '()'.
--
-- >>> run_ $ cmd "echo" & addArgs ["Hello, World!"]
-- Hello, World!
run_ :: (MonadIO m) => ProcessConfiguration -> m ()
run_ = run
