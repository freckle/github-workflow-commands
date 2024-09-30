# github-workflow-commands

For printing workflow commands in GitHub Actions.

See [Workflow commands for GitHub Actions](https://docs.github.com/en/actions/using-workflows/workflow-commands-for-github-actions).

The code herein is based on [GitHub Actions Toolkit](https://github.com/actions/toolkit/tree/main/packages/core/src).

---

<!--
```haskell
module Main (main) where

import Prelude

import Text.Markdown.Unlit ()
```
-->

```haskell
import qualified GitHub.Workflow.Command.Annotation as GH
import Control.Lens
```

An annotation is at minimum just a string.

```haskell
example1 :: IO ()
example1 =
  GH.printByteStringLn $
    GH.error "Something failed."
```

An annotation can also include a location.

```haskell
someLocation :: GH.Location
someLocation =
  GH.inFile "app.js"
    & GH.position ?~
        ( GH.atLine 13
            & GH.extent ?~ GH.ToLine 16
        )
```

```haskell
example2 :: IO ()
example2 =
  GH.printByteStringLn $
    GH.warning "Something seems amiss here."
      & GH.location ?~ someLocation
```

<!--
```haskell
main :: IO ()
main = example1 >> example2
```
-->

---

[CHANGELOG](./CHANGELOG.md) | [LICENSE](./LICENSE)
