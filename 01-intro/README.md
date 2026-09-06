# Installing scala-cli:

https://scala-cli.virtuslab.org/docs/overview#installation

You do not need to install Java or Scala. The scala-cli tool takes
care of all dependencies, and leaves the Java setup on your
machine intact.

# Using scala-cli

Call the following commands to work with this project:

scala-cli compile . - to compile the project
scala-cli test .    - to compile and test (add "-w" as an option,
                      to recompile and test, every time you change
                      sources)
scala-cli repl .    - to compile the project, load it into repl,
                      and open a prompt, so that you can play with
                      it
scala-cli clean .   - to remove scala-cli generated files

Note the period after all commands (it represents the current
directory).

The following command will run the project. The -M option is only
needed if there is more than one main method in the project.

```
scala-cli run . -M adpro.intro.printAbs
```

More info at: https://scala-cli.virtuslab.org/docs/overview/

# Working on the exercises

To work on the exercises it is best to run `scala-cli test . -w`
in a terminal, and then open `Exercises.scala` in an editor. Solve
the exercises replacing `???` with your solution. Every time you
save, the results of testing are updated in the terminal.

Compile and test frequently. Best continuously.

The text of the exercises is in the PDF file in the directory.

The tests are in `Exercises.test.scala`. For starters you can ignore
them.  But when a test fails, the first thing to debug it, is to read
and understand the test. This normally gives you a hint how to fix a
problem (Of course you can also ask for help!).

There will be a class on writing these tests roughly mid-course, so
things will get considerably less cryptic over time.

To run a single test, say "Ex10.01" use filtering (drop `-w` if you do not need
watching):
```
scala-cli test . -w -- -f Ex18
```
Any substring from the test name can be used with `-f`.


# Other files

`Factorial.scala` shows a loop implementation (imperative) and two
recursive implementations of the factorial example for self-study.
`Purification.scala` has the same code with a bit more commentary.

# Software Versions

In this course we are using:

```
openjdk    v. 21.0.12 you need to install, other versions might work (openjdk
                      is only used to run scala-cli, the compiler and your
                      programs are run with jvm managed by scala-cli
                      automatically, this is jvm 25 this year)
scala-cli  v. 1.15.0  you need to install this
scala      v. 3.8.4   installed and managed automatically by scala-cli
scalacheck v. 1.19.0  installed and managed automatically by scala-cli
```
