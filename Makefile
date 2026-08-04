SBCL := sbcl --noinform --non-interactive
BUILD_DIR := build
TARGET := $(BUILD_DIR)/cl-toolkit

.PHONY: all build install clean smoke-test setup help

all: build

help:
	@echo "cl-toolkit build targets:"
	@echo "  make build        - Build binary via ASDF to build/cl-toolkit"
	@echo "  make clean        - Remove compiled artifacts"
	@echo "  make smoke-test   - Quick CLI smoke test"
	@echo "  make help         - Show this help"

build: $(wildcard src/*.lisp) cl-toolkit.asd
	@mkdir -p $(BUILD_DIR)
	$(SBCL) --eval '(ql:quickload :asdf)' \
	        --eval '(push #P"./" asdf:*central-registry*)' \
	        --eval '(asdf:operate (quote asdf:program-op) :cl-toolkit/bin)' \
	        --eval '(ext:quit)'
	mv -f cl-toolkit $(TARGET)

smoke-test: $(TARGET)
	@echo "--- Smoke test ---"
	@./$(TARGET) version
	@./$(TARGET) parse --code "(+ 1 2)" > /dev/null
	@echo "--- All smoke tests passed ---"

setup: build
	@./setup.sh

install: build
	sudo cp $(TARGET) /usr/local/bin/

clean:
	rm -rf $(BUILD_DIR)
	rm -f cl-toolkit
	rm -f *.fasl *.dx64fsl *.lx64fsl
	rm -f src/*.fasl src/*.dx64fsl src/*.lx64fsl
	rm -f test/*.fasl test/*.dx64fsl test/*.lx64fsl
