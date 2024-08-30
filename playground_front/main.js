import './style.css';

async function init() {
  const btn = document.getElementById('run');
  const output_area = document.getElementById('output_area');
  const input_area = document.getElementById('input_area');
  if (!btn || !output_area || !input_area) {
    throw new Error();
  }

  const compiler = await (async () => {
    try {
      const res = await fetch('/compiler.wasm');
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
      const encoder = new TextEncoder();
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
        const decoder = new TextDecoder();
        output_area.value = decoder.decode(output);
        return;
      }
      // if succeeded, load and execute
      const generaged = await WebAssembly.instantiate(output, {
        // stdlib!
        std: {
          print: (...arg) => (output_area.value += `${arg}`),
          println: (...arg) => (output_area.value += `${arg}\n`),
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
      const retval_compiled = generaged.instance.exports.main();
      output_area.value += 'result=' + retval_compiled;
    } catch (e) {
      output_area.value += '[error] failed to execute : ' + e;
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
