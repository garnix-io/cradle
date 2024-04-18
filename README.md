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

# Alternatives

- [`process`](https://hackage.haskell.org/package/process): `process` is substantially lower level than `cradle`. That is, it allows for more control, and more options, but at the cost of a more complicated interface. (`cradle` uses `process` under the hood.)
- [`typed-process`](https://hackage.haskell.org/package/typed-process): A safer version of `process`. Still lower-level than `cradle`.
- [`turtle`](https://hackage.haskell.org/package/turtle): `turtle` is a more ambitious library, including a number of system-related functionality (such as changing directories, looking up environment variables, etc.), as well as providing convenient functions for running common executables such as `date` and `rm`. `cradle` by contrast *only* runs specified processes.
- `shake`'s `cmd`. This was the basis for `cradle`, which indeed started it's life as a factoring-out of the `cmd` logic into a separate library. Since then, the API has been cleaned up and generalized. Notable changes are separating `cmd` from `run` so functions can be more easily applied to the command; not being variadic in the arguments for better type inference; and removing `CmdArguments` class altogether in favor of a composition of `ProcessConfiguration -> ProcessConfiguration` functions.
