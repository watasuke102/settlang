COMPILER = playground_front/public/compiler.wasm
BACKEND_SRC = $(shell find backend -name '*.rs')
all: playground
playground: $(COMPILER)

$(COMPILER): playground_compiler/src/lib.rs $(BACKEND_SRC)
	cargo build --target=wasm32-unknown-unknown --release -p playground_compiler
	@cp -v target/wasm32-unknown-unknown/release/playground_compiler.wasm $@

test:
	cargo test --workspace --exclude playground_compiler

clean:
	cargo clean
	rm $(COMPILER)

.PHONY: all playground clean test
