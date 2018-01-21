# levin
SMT constraint analysis for grammar learning

## Dependencies

scala-smtlib

## Usage

```createDisjointAssertions```: Function that can convert some expression the form ```(and(and(and....``` to a list of expression.

```stripLets``` : Removes all the declarations by "let statements" from a term

```createLetFcn``` : Returns a higher order function of type ```Term -> Term``` that can be used to add the declarations back in. 
