# Settlang

Settlang is a statically-typed programming language that express mutability by whether variables have setter.

## Let's try!

Visit [Playground (watasuke102.github.io/settlang)](https://watasuke102.github.io/settlang/) to try on the playground!

## Example

See `/examples` for more.

```
#*
  Settlang minimal example
*#
fn accumulator(self: i64, e: i64) -> i64 {
  # just return added value
  ret self + e
}

fn randomize() -> i64 {
  ret random(0, 100)
}

# function named `main` is entrypoint
fn main() -> i64 {
  # they have 'setter' function, so mutable
  let sum: i64 | accumulator = 0
  let rnd: i64 | randomize   = 0

  for i in 0..10 {
    rnd.set()         # call setter of rnd
    print("{}", rnd)  # print value
    if i != 9 {
      print(", ")
    }
    sum.set(sum, rnd) # call setter of sum
  }
  println("")

  ret sum
}

```

## LICENSE

Dual-licensed; MIT (`LICENSE-MIT` or [The MIT License – Open Source Initiative](https://opensource.org/license/mit/)) or MIT SUSHI-WARE LICENSE (`LICENSE-MIT_SUSHI.md`)
