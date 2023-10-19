# ion

`ion` is tiny expression-based scripting language. It's small in both codebase size and syntax, but not in capability.

## usage

```sh
# open a repl
go run bin/main.go

# run a file
go run bin/main.go <file>
```

## the language

```go
fib := n -> {
  if n > 1 {
    return fib(n - 1) + fib(n - 2)
  } else {
    return n
  }
}

for i in [ 1, 2, 3, 4, 5 ] {
  print(i * i)
}
```

## inspiration

- [`oak`](https://github.com/thesephist/oak)
- [`monkey`](https://interpreterbook.com)
