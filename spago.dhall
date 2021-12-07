{-
Welcome to a Spago project!
You can edit this file as you like.

Need help? See the following resources:
- Spago documentation: https://github.com/purescript/spago
- Dhall language tour: https://docs.dhall-lang.org/tutorials/Language-Tour.html

When creating a new Spago project, you can use
`spago init --no-comments` or `spago init -C`
to generate this file without the comments in this block.
-}
{ name = "rmrk-parser"
, dependencies =
  [ "aff"
  , "argonaut-codecs"
  , "argonaut-core"
  , "argonaut-generic"
  , "arrays"
  , "bigints"
  , "console"
  , "control"
  , "debug"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "foreign-object"
  , "integers"
  , "lists"
  , "math"
  , "maybe"
  , "ordered-collections"
  , "prelude"
  , "psci-support"
  , "spec"
  , "strings"
  , "transformers"
  , "tuples"
  , "validation"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
, license = "(GPL-3.0-only OR MIT)"
, repository = "https://github.com/rmrk-team/purescript-rmrk-parser.git"
, author = "Jørn Andre, @yornaath | @gorillatron | @rmrk.app"
}
