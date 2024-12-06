# ghci -> load a module:

```
ghci> :load Main
ghci> foobar
```

# ghcid -> run repl reloading on file changes:

```
~/.local/bin/ghcid --c "ghci Main.hs" --test "main"
```

# haskell tools:

- ghc
- ghci
- ghcid
- hlint
- ormolu
- hls
- cabal
- stack

# hidden packages:

some packages are hidden by default in the ghci repl.
Ie. they are installed, but not visible; not available to be imported.
