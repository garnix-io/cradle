# `cradle`

Conveniently run child processes:

```haskell
{-# LANGUAGE ScopedTypeVariables #-}
import Cradle

main :: IO ()
main = do
  StdoutTrimmed stdout <- run $ cmd "echo" & addArgs ["Hello, World!"]
  print stdout
  -- prints "Hello, World!"

  -- `run` is polymorphic on the return type. Just by adding it to the pattern
  -- match, you can get things like the exit code and stderr:
  (exitCode :: ExitCode, Stderr stderr) <- run $ cmd "ls" & addArgs ["does-not-exist"]
  print exitCode
  -- prints ExitFailure 2
  print stderr
  -- prints "ls: cannot access 'does-not-exist': No such file or directory\n"
```

It does _not_ run the processes through a shell, but rather is meant as a high-level interface to `fork` & `exec`.
