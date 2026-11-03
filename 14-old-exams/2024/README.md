
Start in 'Exam.scala'

CREDITS:

The files `Foldable.scala`, `Gen.scala`, `LazyList.scala`,
`Monad.scala`, `Monoid.scala`, `Par.scala`, `Parsing.scala`, `State.scala` come
from the text book repository (fpinscala 2nd edition).  They have been
slightly trimmed and adjusted to simplify dependencies.

The files `Dist.scala`, `IData.scala`, `Name.scala`, `Pigaro.scala`
come from the Pigaro project, (c) Andrzej Wąsowski.

RUNNING TESTS:

To run tests on my solutions do:

```
scala-cli test . -- -f _
```

Filtering on underscore removes the spurious tests created by mangling
the tests in solution.  To test your solution, edit the package
directive of the test file to place it in the same package as
Exam.scala
