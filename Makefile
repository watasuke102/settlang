PUBLIC_DIR = playground_front/public
COMPILER = $(PUBLIC_DIR)/compiler.wasm
BACKEND_SRC = $(shell find backend -name '*.rs')
all: playground
playground: $(COMPILER)
	cd playground_front && npm run build

$(COMPILER): playground_compiler/src/lib.rs $(BACKEND_SRC) $(PUBLIC_DIR)
	cargo build --target=wasm32-unknown-unknown --release -p playground_compiler
	@cp -v target/wasm32-unknown-unknown/release/playground_compiler.wasm $@

$(PUBLIC_DIR):
	mkdir -p $@

test:
	cargo test --workspace --exclude playground_compiler

clean:
	cargo clean
	rm $(COMPILER)

.PHONY: all playground clean test

%.wasm: %.stt
	cargo run -p cli_compiler -- --verbose -o $@ $<
