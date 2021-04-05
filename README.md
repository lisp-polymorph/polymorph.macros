# polymorph.utility
## Macros and Utility

- Default
Returns a reasonable default object for a given type.

- Bind
Unites 3 things: `let*`, `multiple-value-bind` and builtin type declarations. Uses default for filling out the values if types was provided, otherwise defaults to `nil`.

Examples of usage:

``` common-lisp
(tbind* ((x :t fixnum 10)   ; x is 10
         (y :t string)      ; y is an empty vector of characters
         (z (random 42))    ; z is exactly what it is supposed to be
         ((a :t integer b :t (integer 0 56)) (floor 179 57)))  ; a and b are 3 and 8 respectively
  body)
```
