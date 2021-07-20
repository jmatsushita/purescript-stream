{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "purescript-stream"
, dependencies = 
  [ "arrays"
  , "console"
  , "effect"
  , "psci-support"
  , "free"
  , "functors"
  , "partial"
  , "profunctor"
  , "generics-rep" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
