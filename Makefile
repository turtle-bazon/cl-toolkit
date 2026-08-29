SBCL := sbcl --noinform --non-interactive
BUILD_DIR := build
TARGET := $(BUILD_DIR)/cl-toolkit

.PHONY: all build install clean test smoke-test setup help

all: build

help:
	@echo "cl-toolkit build targets:"
	@echo "  make build        - Build binary via ASDF to build/cl-toolkit"
	@echo "  make clean        - Remove compiled artifacts"
	@echo "  make test         - Run FiveAM unit/regression tests"
	@echo "  make smoke-test   - Quick CLI smoke test"
	@echo "  make ci           - Per-command CLI matrix (29 assertions)"
	@echo "  make help         - Show this help"

build: $(wildcard src/*.lisp) cl-toolkit.asd
	@mkdir -p $(BUILD_DIR)
	$(SBCL) --eval '(ql:quickload :asdf)' \
	        --eval '(push #P"./" asdf:*central-registry*)' \
	        --eval '(asdf:operate (quote asdf:program-op) :cl-toolkit/bin)' \
	        --eval '(ext:quit)'

ci: build
	@bash test/cli-matrix.sh ./build/cl-toolkit

smoke-test: $(TARGET)
	@echo "--- Smoke test ---"
	@./$(TARGET) version
	@./$(TARGET) parse --code "(+ 1 2)" > /dev/null
	@echo "--- All smoke tests passed ---"

# Wipe the fasl cache first: stale compiled rules/functions have caused
# phantom behavior differences between source and binary.
test:
	@rm -rf ~/.cache/common-lisp/sbcl-*/tmp/cl-toolkit
	$(SBCL) --eval '(ql:quickload :asdf)' \
	        --eval '(push #P"./" asdf:*central-registry*)' \
	        --eval '(ql:quickload :cl-toolkit/tests)' \
	        --eval '(unless (fiveam:run! :cl-toolkit) (uiop:quit 1))' \
	        --eval '(uiop:quit)'

setup: build
	@./setup.sh

install: build
	sudo cp $(TARGET) /usr/local/bin/

clean:
	rm -rf $(BUILD_DIR)
	rm -f *.fasl *.dx64fsl *.lx64fsl
	rm -f src/*.fasl src/*.dx64fsl src/*.lx64fsl
	rm -f test/*.fasl test/*.dx64fsl test/*.lx64fsl
