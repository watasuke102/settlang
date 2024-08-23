COMPILER = playground_front/public/compiler.wasm
all: playground
playground: $(COMPILER)

$(COMPILER): playground_compiler/src/lib.rs
	cargo build --target=wasm32-unknown-unknown --release -p playground_compiler
	@cp -v target/wasm32-unknown-unknown/release/playground_compiler.wasm $@

clean:
	cargo clean
	rm $(COMPILER)

.PHONY: all playground clean
