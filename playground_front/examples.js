function append_examples() {
  const input_area = document.getElementById('input_area');
  const example_list = document.getElementById('example_list');

  if (!input_area || !example_list) {
    throw new Error('Elements not found');
  }

  input_area.value = examples.Default ?? '';

  Object.keys(examples).forEach(key => {
    const btn = document.createElement('button');
    btn.innerText = key;
    btn.addEventListener('click', () => {
      input_area.value = examples[key];
    });
    example_list.appendChild(btn);
  });
}

const examples = {
  Default: `#*
  Settlang minimal example
*#
fn accumulator(self: i64, e: i64) -> i64 {
  # just return added value
  ret self + e
}

fn randomize() -> i64 {
  ret random(0, 100)
}

# function named \`main\` is entrypoint
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
`,
  FizzBuzz: `fn main() {
  let max: i64 = read()
  for i in 1..=max {
    if i%15 == 0 {
      print("FizzBuzz")
    } else if i%3 == 0 {
      print("Fizz")
    } else if i%5 == 0 {
      print("Buzz")
    } else {
      print("{}", i)
    }
    print(", ")
  }
  println("")
}
`,
  GuessNumber: `fn main() -> i32 {
  let min: i64 = 0
  let max: i64 = 100
  let ans: i64 = random(min, max)
  alert("Guess the number ({} <= answer <= {})", min, max)

  let input: i64 | read_user_input = 0
  let try_max: i64 = 5
  for i in 0..try_max {
    input.set()
    print("Try {}/3, your input={}", i+1, input)
    if input == ans {
      alert("Exactly!")
      println("-> You win!")
      ret 0
    } else if input < ans {
      alert("Too small")
      println(" -> Too small")
    } else {
      alert("Too big")
      println(" -> Too big")
    }
  }

  println("You lose. answer={}", ans)
  ret 1
}

fn read_user_input() -> i64 { ret read() }
`,
  Setter: `fn main() -> i32 {
  # this variable does not have a setter, so it is immutable
  let immutable: i32 = 10
  
  # this variable has 'double()' as a setter
  let x: i32 | double = 10 # x is 10; initialization and assignment are different
  
  # call setter by <mutable varname>.set(<args>)
  x.set(x) # x will become 20

  # setter (no args)
  let one: i32 | set_one = 0
  one.set() # one will become 1
  
  # of course 'double()' can be used as a normal function
  ret immutable * (x+one) - double(100)
  # returns 10 * (20 + 1) - 100*2 == 204
}

fn double(in: i32) -> i32 {
  ret in*2
}
fn set_one() -> i32 {
  ret 1
}
`,
  ForLoop: `fn add(self: i64, i: i64) -> i64 {
  ret self+i
}

fn main() -> i64 {
  let exclusive: i64 | add = 0
  for i in 1..14 {
    exclusive.set(exclusive, i)
  }
  
  let inclusive: i64 | add = 0
  for i in 1..=13 {
    inclusive.set(inclusive, i)
  }

  ret if inclusive == exclusive {
    inclusive
  } else {
    0
  }
}
`,
};

append_examples();
