import './style.css';

const btn = document.getElementById('run');
const output_area = document.getElementById('output_area');
const input_area = document.getElementById('input_area');
if (!btn || !output_area || !input_area) {
  throw new Error('Elements not found');
}

const encoder = new TextEncoder();
const decoder = new TextDecoder();
let mem_viewer;
function get_str_by_offset(str_ptr) {
  const buffer = [];
  let i = BigInt(8); // data section is loaded 0x08
  while (true) {
    const c = mem_viewer.getUint8(Number(str_ptr + i));
    if (c == 0) {
      break;
    }
    buffer.push(c);
    ++i;
  }
  const array = new Uint8Array(buffer);
  return decoder.decode(array);
}
function format(str_ptr, ...args) {
  let result = '';
  let str_splited = get_str_by_offset(str_ptr).split('{}');
  for (let i = 0; i < str_splited.length; ++i) {
    result += str_splited[i];
    if (i !== str_splited.length - 1) {
      result += args[i] ?? '';
    }
  }
  return result;
}

async function init() {
  const compiler = await (async () => {
    try {
      const res = await fetch('/settlang/compiler.wasm');
      if (!res.ok || !res.body || res.body instanceof ReadableStream === false) {
        output.innerText = 'Failed to load : invalid response';
        return null;
      }
      return WebAssembly.instantiateStreaming(res);
    } catch (e) {
      btn.innerText = 'Failed to load';
      output.innerText = 'Failed to load : ' + e;
      return null;
    }
  })();
  if (!compiler) {
    return;
  }

  const execute = async () => {
    try {
      // get input and store it to memory
      output_area.value = '';
      const input = encoder.encode(input_area.value + ' ');
      const input_ptr = 16;
      {
        const buffer = new Uint8Array(compiler.instance.exports.memory.buffer);
        buffer.set(input, input_ptr);
      }
      // build source code
      const retval_build = compiler.instance.exports.build(input_ptr, input.length);
      // read results
      const viewer = new DataView(compiler.instance.exports.memory.buffer);
      const is_succeed = viewer.getUint32(retval_build + 0) === 1; // 0:3
      const output_len = viewer.getUint32(retval_build + 4); // 4:7
      const output = (() => {
        const buffer = [];
        for (let i = 0; i < output_len; ++i) {
          const c = viewer.getUint8(retval_build + 8 + i);
          buffer.push(c);
        }
        return new Uint8Array(buffer);
      })();
      // if failed to compile, show it and return
      if (!is_succeed) {
        output_area.value = decoder.decode(output);
        return;
      }
      // if succeeded, load and execute
      const generaged = await WebAssembly.instantiate(output, {
        // stdlib!
        std: {
          print: (str_ptr, ...arg) => {
            output_area.value += format(str_ptr, ...arg);
          },
          println: (str_ptr, ...arg) => {
            output_area.value += format(str_ptr, ...arg) + '\n';
          },
          alert: (str_ptr, ...arg) => alert(format(str_ptr, ...arg)),
          read: () => {
            try {
              const user_input = window.prompt('Please enter a number') ?? '0';
              return BigInt(user_input);
            } catch (e) {
              return BigInt(0);
            }
          },
          random: (min, max) => {
            try {
              const max_n = Number(max);
              const min_n = Number(min);
              const rand = Math.floor(Math.random() * (max_n - min_n) + min_n);
              return BigInt(rand);
            } catch (e) {
              return BigInt(0);
            }
          },
        },
      });
      mem_viewer = new DataView(generaged.instance.exports.memory.buffer);
      const retval_compiled = generaged.instance.exports.main();
      output_area.value += 'result=' + retval_compiled;
    } catch (e) {
      output_area.value += '[error] failed to execute : ' + e;
      throw e;
    }
  };
  btn.removeAttribute('disabled');
  btn.innerText = 'Run (Ctrl+Enter)';
  btn.onclick = execute;
  input_area.addEventListener('keydown', e => {
    if (e.ctrlKey && e.code === 'Enter') {
      execute();
    }
  });
}

init();
