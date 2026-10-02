SBCL := sbcl --noinform --non-interactive
BUILD_DIR := build
TARGET := $(BUILD_DIR)/cl-toolkit

# ASDF stores fasls under the XDG cache root, mirroring the absolute source
# path, e.g. ~/.cache/common-lisp/sbcl-<version>/home/user/proj/src/*.fasl.
# Wiping the wrong subtree silently leaves stale fasls in place.
FASL_CACHE := $(wildcard $(HOME)/.cache/common-lisp/sbcl-*/$(CURDIR))

.PHONY: all build rebuild install clean clean-cache test smoke-test setup help

all: build

help:
	@echo "cl-toolkit build targets:"
	@echo "  make build        - Build binary via ASDF to build/cl-toolkit"
	@echo "  make rebuild      - Discard fasl cache + binary, full rebuild"
	@echo "  make clean        - Remove compiled artifacts"
	@echo "  make clean-cache  - Remove this project's fasl cache only"
	@echo "  make test         - Run FiveAM unit/regression tests"
	@echo "  make smoke-test   - Quick CLI smoke test"
	@echo "  make ci           - Per-command CLI matrix + compile gates"
	@echo "  make help         - Show this help"

build: $(wildcard src/*.lisp) cl-toolkit.asd
	@mkdir -p $(BUILD_DIR)
	$(SBCL) --eval '(ql:quickload :asdf)' \
	        --eval '(push #P"./" asdf:*central-registry*)' \
	        --eval '(asdf:operate (quote asdf:program-op) :cl-toolkit/bin)' \
	        --eval '(uiop:quit)'

# Full, honest build: no fasl reuse, so every warning is visible.
rebuild:
	$(MAKE) clean-cache
	rm -rf $(BUILD_DIR)
	$(MAKE) build

clean-cache:
	rm -rf $(FASL_CACHE)

ci: build
	@bash test/cli-matrix.sh ./build/cl-toolkit

smoke-test: $(TARGET)
	@echo "--- Smoke test ---"
	@./$(TARGET) version
	@./$(TARGET) parse --code "(+ 1 2)" > /dev/null
	@echo "--- All smoke tests passed ---"

# Wipe the fasl cache first: stale compiled rules/functions have caused
# phantom behavior differences between source and binary.
test: clean-cache
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
