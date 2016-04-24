# dsvt

Delimiter-separated value (DSV) templating. 

Takes a stream of DSV lines on STDIN, plus a HTML/XML template, and outputs one
template unit per line.

The template is parsed by HXT, and thus must be valid HTML or XML.

```
# template.html
<p><b>{{ $1 }}</b> <span>{{ $2 }}</span></p>
```

```
# input.txt
banana $2.00
apple $3.00
beer $5.00
```

Command:

    cat input.txt | dsvt template.html
    # output
    <p><b>banana</b> <span>$2.00</span></p>
    <p><b>apple</b> <span>$3.00</span></p>
    <p><b>beer</b> <span>$5.00</span></p>

Directives:

* ng-class
* ng-show
* ng-hide
* ng-bind

TBA

### dsvt-test

This tool lets you test dsvt expressions.

```
dsvt-test

Usage: dsvt-test EXPRESSION [-b] [FIELDS]
  DSV templating

Available options:
  -h,--help                Show this help text
  EXPRESSION               Expression
  -b                       Evaluate to Bool; default String
  FIELDS                   Test field values, space-delimited
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
dsvt $ dsvt-test '$1' 'apple'
FieldNum 1
"apple"
dsvt-test  '$1 == "apple"' 'apple banana'
Compare "==" (FieldNum 1) (LiteralExpr (LitString "apple"))
"True"
dsvt $ dsvt-test -b '$1 == "apple"' 'apple banana'
Compare "==" (FieldNum 1) (LiteralExpr (LitString "apple"))
True
dsvt $ dsvt-test -b '$2 == "apple"' 'apple banana'
Compare "==" (FieldNum 2) (LiteralExpr (LitString "apple"))
False
dsvt $ dsvt-test -b '$2 == "banana"' 'apple banana'
Compare "==" (FieldNum 2) (LiteralExpr (LitString "banana"))
True
dsvt $ dsvt-test '{apple: 10 > 1, banana: 3 > 0}'
StringChoice (fromList [("apple",Compare ">" (LiteralExpr (LitNumber 10.0)) (LiteralExpr (LitNumber 1.0))),("banana",Compare ">" (LiteralExpr (LitNumber 3.0)) (LiteralExpr (LitNumber 0.0)))])
"apple banana"
```

Be careful about Bash quoting. Use single quotes if you want to use a
field number expression like `$1`, as with `awk`.
