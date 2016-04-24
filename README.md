# dsvt: 

Delimiter-separated value (DSV) templating. 

Takes a stream of DSV lines on STDIN, plus a HTML/XML template, and outputs one
template unit per line.


### dsvt-test

This tool lets you test dsvt expressions.

```
Usage: dsvt-test EXPRESSION [-b] [FIELDS]
  DSV templating
```

Examples:

```
dsvt $ dsvt-test -- "-10"
LiteralExpr (LitNumber (-10.0))
"-10.0"
LiteralExpr (LitNumber 10.0)
dsvt $ dsvt-test -b "10 == 10"
Compare "==" (LiteralExpr (LitNumber 10.0)) (LiteralExpr (LitNumber 10.0))
True
dsvt $ dsvt-test -b "10 > 10"
Compare ">" (LiteralExpr (LitNumber 10.0)) (LiteralExpr (LitNumber 10.0))
False
```

