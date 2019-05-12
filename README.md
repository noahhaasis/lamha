# An interpreter for the Lambda Calculus
Terms have to have the following form:
```
\x.M  | Abstraction
(M N) | Application
x     | Variable
```

# Examples
const</br>
`\a.\b.a`</br>
</br>
omega (infinite recursion -> doesn't terminate)</br>
`(\f.(f f) \f.(f f))`</br>
</br>
y-combinator</br>
`\f.(\x.(f (x x)) \x.(f (x x)))`</br>
</br>
addition (on church numerals)</br>
`\n.\f.\x.(n (f x))`
