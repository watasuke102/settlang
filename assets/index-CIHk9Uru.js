(function(){const i=document.createElement("link").relList;if(i&&i.supports&&i.supports("modulepreload"))return;for(const e of document.querySelectorAll('link[rel="modulepreload"]'))n(e);new MutationObserver(e=>{for(const r of e)if(r.type==="childList")for(const u of r.addedNodes)u.tagName==="LINK"&&u.rel==="modulepreload"&&n(u)}).observe(document,{childList:!0,subtree:!0});function t(e){const r={};return e.integrity&&(r.integrity=e.integrity),e.referrerPolicy&&(r.referrerPolicy=e.referrerPolicy),e.crossOrigin==="use-credentials"?r.credentials="include":e.crossOrigin==="anonymous"?r.credentials="omit":r.credentials="same-origin",r}function n(e){if(e.ep)return;e.ep=!0;const r=t(e);fetch(e.href,r)}})();const c=document.getElementById("run"),l=document.getElementById("output_area"),p=document.getElementById("input_area");if(!c||!l||!p)throw new Error("Elements not found");const E=new TextEncoder,g=new TextDecoder;let v;function z(a){const i=[];let t=BigInt(8);for(;;){const e=v.getUint8(Number(a+t));if(e==0)break;i.push(e),++t}const n=new Uint8Array(i);return g.decode(n)}function m(a,...i){let t="",n=z(a).split("{}");for(let e=0;e<n.length;++e)t+=n[e],e!==n.length-1&&(t+=i[e]??"");return t}async function B(){const a=await(async()=>{try{const t=await fetch("/settlang/compiler.wasm");return!t.ok||!t.body||!(t.body instanceof ReadableStream)?(output.innerText="Failed to load : invalid response",null):WebAssembly.instantiateStreaming(t)}catch(t){return c.innerText="Failed to load",output.innerText="Failed to load : "+t,null}})();if(!a)return;const i=async()=>{try{l.value="";const t=E.encode(p.value+" "),n=16;new Uint8Array(a.instance.exports.memory.buffer).set(t,n);const e=a.instance.exports.build(n,t.length),r=new DataView(a.instance.exports.memory.buffer),u=r.getUint32(e+0)===1,h=r.getUint32(e+4),b=(()=>{const o=[];for(let s=0;s<h;++s){const d=r.getUint8(e+8+s);o.push(d)}return new Uint8Array(o)})();if(!u){l.value=g.decode(b);return}const y=await WebAssembly.instantiate(b,{std:{print:(o,...s)=>{l.value+=m(o,...s)},println:(o,...s)=>{l.value+=m(o,...s)+`
`},alert:(o,...s)=>alert(m(o,...s)),read:()=>{try{const o=window.prompt("Please enter a number")??"0";return BigInt(o)}catch{return BigInt(0)}},random:(o,s)=>{try{const d=Number(s),x=Number(o),_=Math.floor(Math.random()*(d-x)+x);return BigInt(_)}catch{return BigInt(0)}}}});v=new DataView(y.instance.exports.memory.buffer);const w=y.instance.exports.main();l.value+="result="+w}catch(t){throw l.value+="[error] failed to execute : "+t,t}};c.removeAttribute("disabled"),c.innerText="Run (Ctrl+Enter)",c.onclick=i,p.addEventListener("keydown",t=>{t.ctrlKey&&t.code==="Enter"&&i()})}B();function T(){const a=document.getElementById("input_area"),i=document.getElementById("example_list");if(!a||!i)throw new Error("Elements not found");a.value=f.Default,Object.keys(f).forEach(t=>{const n=document.createElement("button");n.innerText=t,n.addEventListener("click",()=>{a.value=f[t]}),i.appendChild(n)})}const f={Default:`#*
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
`};T();
