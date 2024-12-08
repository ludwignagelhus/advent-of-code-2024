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

# packages and installing them

package names seem to be case-sensitive?
seems to depend on the package's name in the registry.
...for which there doesn't appear to be a strict naming convention;
-> for example
cabal install hspec and `import Test.Hspec`
cabal install QuickCheck and `import Test.QuickCheck`
