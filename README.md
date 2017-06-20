## minicom

minicom is a compiler of mini toy language written in Rust using LLVM.

### Examples

Try `$ cargo run -- examples/fib.mini`. It will emit a binary executable, `fib`.

```scala
def fib(n: Int): Int = {
  if n <= 1 {
    1
  } else {
    fib(n-1) + fib(n-2)
  }
}

def main() = {
  let n = ref(0)
  while @n < 38 {
    print(fib(@n))
    n <- @n + 1
  }
}
```

### Features

- Functions
- Variables
- Arithmetic Operations
- Control Flow (`if` and `while`)
- Mutable Reference (`ref`)

