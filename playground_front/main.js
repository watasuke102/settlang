import './style.css';

async function init() {
  const btn = document.getElementById('run');
  const output_area = document.getElementById('output_area');
  if (!btn || !output_area) {
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

  btn.removeAttribute('disabled');
  btn.innerText = 'Run';
  btn.onclick = async () => {
    try {
      // get input and store it to memory
      const input_area = document.getElementById('input_area');
      output_area.innerHTML = '';
      const encoder = new TextEncoder();
      const input = encoder.encode(input_area.value);
      const input_ptr = 16;
      {
        const buffer = new Uint8Array(compiler.instance.exports.memory.buffer);
        buffer.set(input, input_ptr);
      }
      // build source code
      const retval = compiler.instance.exports.build(input_ptr, input.length);
      // read results
      const viewer = new DataView(compiler.instance.exports.memory.buffer);
      const is_succeed = viewer.getUint32(retval + 0) === 1; // 0:3
      const output_len = viewer.getUint32(retval + 4); // 4:7
      const output = (() => {
        const buffer = [];
        for (let i = 0; i < output_len; ++i) {
          const c = viewer.getUint8(retval + 8 + i);
          buffer.push(c);
        }
        return new Uint8Array(buffer);
      })();
      // if failed, show it and return
      if (!is_succeed) {
        const decoder = new TextDecoder();
        output_area.innerHTML = decoder.decode(output);
        return;
      }
      // if succeeded, load and execute
      const generaged = await WebAssembly.instantiate(output, {
        /* TODO: stdlib */
      });
      output_area.innerHTML = 'result=' + generaged.instance.exports.main();
    } catch (e) {
      output_area.innerHTML = '[error] failed to execute : ' + e;
    }
  };
}

init();
