(function(){let e=document.createElement(`link`).relList;if(e&&e.supports&&e.supports(`modulepreload`))return;for(let e of document.querySelectorAll(`link[rel="modulepreload"]`))n(e);new MutationObserver(e=>{for(let t of e)if(t.type===`childList`)for(let e of t.addedNodes)e.tagName===`LINK`&&e.rel===`modulepreload`&&n(e)}).observe(document,{childList:!0,subtree:!0});function t(e){let t={};return e.integrity&&(t.integrity=e.integrity),e.referrerPolicy&&(t.referrerPolicy=e.referrerPolicy),e.crossOrigin===`use-credentials`?t.credentials=`include`:e.crossOrigin===`anonymous`?t.credentials=`omit`:t.credentials=`same-origin`,t}function n(e){if(e.ep)return;e.ep=!0;let n=t(e);fetch(e.href,n)}})();var e=document.getElementById(`run`),t=document.getElementById(`output_area`),n=document.getElementById(`input_area`);if(!e||!t||!n)throw Error(`Elements not found`);var r=new TextEncoder,i=new TextDecoder,a;function o(e){let t=[],n=BigInt(8);for(;;){let r=a.getUint8(Number(e+n));if(r==0)break;t.push(r),++n}let r=new Uint8Array(t);return i.decode(r)}function s(e,...t){let n=``,r=o(e).split(`{}`);for(let e=0;e<r.length;++e)n+=r[e],e!==r.length-1&&(n+=t[e]??``);return n}async function c(){let o=await(async()=>{try{let e=await fetch(`/settlang/compiler.wasm`);return!e.ok||!e.body||!(e.body instanceof ReadableStream)?(output.innerText=`Failed to load : invalid response`,null):WebAssembly.instantiateStreaming(e)}catch(t){return e.innerText=`Failed to load`,output.innerText=`Failed to load : `+t,null}})();if(!o)return;let c=async()=>{try{t.value=``;let e=r.encode(n.value+` `);new Uint8Array(o.instance.exports.memory.buffer).set(e,16);let c=o.instance.exports.build(16,e.length),l=new DataView(o.instance.exports.memory.buffer),u=l.getUint32(c+0)===1,d=l.getUint32(c+4),f=(()=>{let e=[];for(let t=0;t<d;++t){let n=l.getUint8(c+8+t);e.push(n)}return new Uint8Array(e)})();if(!u){t.value=i.decode(f);return}let p=await WebAssembly.instantiate(f,{std:{print:(e,...n)=>{t.value+=s(e,...n)},println:(e,...n)=>{t.value+=s(e,...n)+`
`},alert:(e,...t)=>alert(s(e,...t)),read:()=>{try{let e=window.prompt(`Please enter a number`)??`0`;return BigInt(e)}catch{return BigInt(0)}},random:(e,t)=>{try{let n=Number(t),r=Number(e),i=Math.floor(Math.random()*(n-r)+r);return BigInt(i)}catch{return BigInt(0)}}}});a=new DataView(p.instance.exports.memory.buffer);let m=p.instance.exports.main();t.value+=`result=`+m}catch(e){throw t.value+=`[error] failed to execute : `+e,e}};e.removeAttribute(`disabled`),e.innerText=`Run (Ctrl+Enter)`,e.onclick=c,n.addEventListener(`keydown`,e=>{e.ctrlKey&&e.code===`Enter`&&c()})}c();function l(){let e=document.getElementById(`input_area`),t=document.getElementById(`example_list`);if(!e||!t)throw Error(`Elements not found`);e.value=u.Default??``,Object.keys(u).forEach(n=>{let r=document.createElement(`button`);r.innerText=n,r.addEventListener(`click`,()=>{e.value=u[n]}),t.appendChild(r)})}var u={Default:`#*
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
`,FizzBuzz:`fn main() {
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
`,GuessNumber:`fn main() -> i32 {
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
`,Setter:`fn main() -> i32 {
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
`,ForLoop:`fn add(self: i64, i: i64) -> i64 {
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
`};l();