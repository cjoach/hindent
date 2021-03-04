Haskell pretty printer elm style

## Install

    $ git clone https://github.com/cjoach/hindent-elm.git
    $ cd hindent-elm
    $ ./main install

## Usage

    $ hindent-elm --help
    hindent-elm --version --help [-X<...>]* [<FILENAME>]
    Version 0.1.2
    -X to pass extensions e.g. -XMagicHash etc.

hindent-elm is used in a pipeline style

    $ cat path/to/sourcefile.hs | hindent-elm

## Customization

Create a `.hindent-elm.yaml` file in your project directory or in your
`~/` home directory. The following fields are accepted and are the
default:

``` yaml
line-breaks-before: ["|>"]
line-breaks-after: ["<|"]
extensions:
  - DataKinds
  - GADTs
  - TypeApplications
```

hindent-elm can be forced to insert a newline before specific operators and
tokens with `line-breaks`. This is especially useful when utilizing libraries
like [`servant`](https://www.servant.dev/) which use long type aliases.

Using `extensions`, hindent-elm can be made aware of valid syntactic compiler
extensions that would normally be considered invalid syntax.

It is also possible to specify which extensions hindent-elm runs
with in your `.hindent-elm.yaml`:

```yaml
extensions:
  - MagicHash
  - RecursiveDo
```

## Origin

hindent-elm is a fork of the excellent hindent. The idea of a one and only style
is great but the default style of haskell code in the community is too cramped
for my own taste. I sinply modifed the style to copy the one elm-format does
with elm code.

## Difference in configuration

In going with the elm-format philosophy, I modified the default value of the
options. The line-length is fixed at 80, the indent length is fixed at 4 and the
imports are always sorted. I also added a code-length fixed config at 60 that
further restrict the length of the lines of code. The line-length parameter
restrict the max column of the code, the code-length parameter restricts the
length of the code *without indent*. This means that it will try to break down
lines that have more than 60 characters of useful text and lines that have more
than 80 columns when indents are counted.

The `line-breaks` option was originally used to force line break on some infix
operators. The new option is `line-breaks-before` and a new `line-breaks-after`
was added. The new option forces a line break after the operator instead. This
is used to model the behaviour of elm-format relative to pipe operators (|>,
<|). In elm, with the left pipe (<|), the line break comes after the operator
but with the right pipe (<|), it comes before. I model this behavior by having
two configs. By default, only both these operators are in their respective
lists. I personnaly remap ($) to (<|) and (&) to (|>).

One exception to the rule is if the right side of the infix operation is a do
notation. The do will always be on the same line as the operator with the rest
indented. Another exception is case statements ans multiline lambda. For
indentation consistency, the operator will always be before the line break as to
put the multiline statement on an indentation that is a multiple of 4.

## Caveat

Some choices were made to follow elm-format's ideas even when they don't quite
fit in haskell code. For example, a big `case` statement that doesn't fit on one
line will be reformatted like this

```haskell
case
    long expression...
of
    alternative1 ->
    
    alternative2 ->
```

This follows elm-format's style but is not valid if the case statement is in a
do notation like the following because ghc interprets the `of` as a separate
statement. I prefer to reformat a long expression with more bindings as to not
trigger this unfortunate situation but beware that the project can render your
code uncompilable in these cases. Other cases I observed were multiline
statements wrapped in parentheses since it will align the closing paren with the
opening one on a newline. Same with multiline tuples.

```haskell
function $ do
    case
        long expression...
    of
        alternative1 ->

        alternative2 ->
```

I plan to eventually try to detect these problems and warn the user or simply
alter the design for these special cases but it's not implemented right now.

## Tests

I removed the tests because I was too lazy to modify them to the new style. I
know I will probably be bitten by that and I will probably merge them back
someday but I have more fun things to do for now (like reformat my entire
haskell codebase with this new style).
