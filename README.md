# cl-toolkit

Lisp code toolkit for structural analysis and editing.

## Features

- **Parse**: Parse Lisp source code to JSON AST
- **Validate**: Check syntax and report errors/warnings
- **Find**: Find form at specific line/column
- **Extract**: Extract forms in a range
- **Top-level**: List top-level forms
- **Delete**: Delete form by position or index
- **Insert**: Insert code before/after a form
- **Replace**: Replace form at position
- **Move**: Move form from one position to another
- **Balance**: Analyze parenthesis/bracket balance
- **Format**: Reformat source with consistent indentation
- **Delete-and-Validate**: Delete form and validate result
- **Insert-and-Validate**: Insert code and validate result
- **Replace-and-Validate**: Replace form and validate result

## Build

```bash
make build
```

Produces `build/cl-toolkit`.

## Input Sources

All commands accept input from three sources (in priority order):

1. **`--code`** - Inline code string
2. **`--file`** - Path to a file
3. **stdin** - Piped input

Examples:
```bash
# From file
cl-toolkit parse --file myfile.lisp

# From inline code
cl-toolkit parse --code "(+ 1 2)"

# From stdin
echo "(+ 1 2)" | cl-toolkit parse
cat myfile.lisp | cl-toolkit validate
```

## Usage

```bash
# Parse
cl-toolkit parse --file myfile.lisp
cl-toolkit parse --code "(+ 1 2)"
echo "(+ 1 2)" | cl-toolkit parse

# Validate
cl-toolkit validate --file myfile.lisp
echo "(defun foo () (bar))" | cl-toolkit validate

# Find form at position
cl-toolkit find --file myfile.lisp --line 5 --col 2
cl-toolkit find --code "(defun foo () (bar))" --line 1 --col 2

# List top-level forms
cl-toolkit top-level --file myfile.lisp
echo "(defun foo () (bar))" | cl-toolkit top-level

# Delete form
cl-toolkit delete --file myfile.lisp --line 5 --col 2
cl-toolkit delete --file myfile.lisp --index 0
echo "(defun foo ()) (defun bar ())" | cl-toolkit delete --index 0

# Insert code
cl-toolkit insert --file myfile.lisp --line 5 --col 2 --insert "(new-form)"
echo "(defun foo ())" | cl-toolkit insert --insert "(defun bar ())" --at-end

# Replace form
cl-toolkit replace --file myfile.lisp --line 5 --col 2 --replace "(replaced-form)"
echo "(defun foo ())" | cl-toolkit replace --line 1 --col 1 --replace "(defun bar ())"

# Move form
cl-toolkit move --file myfile.lisp --from-line 5 --from-col 2 --to-line 10 --to-col 2

# Check balance
cl-toolkit balance --file myfile.lisp
echo "(+ 1 2)" | cl-toolkit balance

# Format source
cl-toolkit format --file myfile.lisp
echo "(defun  foo(x)  (+ x 1))" | cl-toolkit format
```

## Output Behavior

- **Read-only commands** (parse, validate, balance, etc.) output JSON to stdout
- **Modification commands** (delete, insert, replace, format) output plain text to stdout when not using `--write`
- **With `--write`**, results are written to the file (creates `.bak` backup)

## Global Setup

```bash
# 1. Clone
git clone git@github.com:turtle-bazon/cl-toolkit.git ~/cl-toolkit

# 2. Install opencode plugin
mkdir -p ~/.config/opencode/tools
cp ~/cl-toolkit/opencode/tools/cl-toolkit.ts ~/.config/opencode/tools/

# 3. Update plugin to use repo path
sed -i "s|path.resolve(__dirname, \"../../build/cl-toolkit\")|\"$HOME/cl-toolkit/build/cl-toolkit\"|" ~/.config/opencode/tools/cl-toolkit.ts

# 4. Add to opencode config (path is expanded during creation)
mkdir -p ~/.config/opencode
cat > ~/.config/opencode/opencode.json << EOF
{
  "tools": {
    "cl-toolkit": {
      "path": "${HOME}/.config/opencode/tools/cl-toolkit.ts"
    }
  }
}
EOF
```

Binary is built automatically on first use.

### Per-Project Setup

Linux/macOS:
```bash
cd /path/to/your/project
~/cl-toolkit/setup.sh .opencode
```

Windows (PowerShell):
```powershell
cd C:\path\to\your\project
C:\cl-toolkit\setup.ps1 .opencode
```

Cross-platform (Node.js):
```bash
cd /path/to/your/project
node ~/cl-toolkit/setup.js .opencode
```

### Agent Usage

Tell the agent:
> Use cl-toolkit to parse/validate/edit Lisp code. Commands: parse, validate, find, extract, top-level, delete, insert, replace, move, balance, format.

Example prompts:
- "Parse this file and show me the top-level forms"
- "Find the form at line 5, column 2"
- "Validate this file for syntax errors"
- "Delete the form at line 10"
- "Insert `(new-form)` after the form at line 5"
- "Move the form from line 3 to after line 8"
- "Check parenthesis balance in this file"
- "Format this file with consistent indentation"

### Available Commands

| Command | Description | Input Sources |
|---------|-------------|---------------|
| `parse` | Parse Lisp code to JSON AST | `--code`, `--file`, or stdin |
| `validate` | Check syntax and report errors | `--code`, `--file`, or stdin |
| `find` | Find form at position | `--code`, `--file`, or stdin + `--line --col` |
| `extract` | Extract forms in a range | `--code`, `--file`, or stdin + `--line1 --col1 --line2 --col2` |
| `top-level` | List top-level forms | `--code`, `--file`, or stdin |
| `delete` | Delete form by position or index | `--code`, `--file`, or stdin + `--line --col` or `--index` |
| `insert` | Insert code before/after a form | `--code`, `--file`, or stdin + `--line --col --insert` or `--at-end` |
| `replace` | Replace form at position | `--code`, `--file`, or stdin + `--line --col --replace` |
| `move` | Move form from one position to another | `--code`, `--file`, or stdin + `--from-line --from-col --to-line --to-col` |
| `balance` | Analyze parenthesis balance | `--code`, `--file`, or stdin |
| `format` | Reformat source with consistent indentation | `--code`, `--file`, or stdin |
| `delete-and-validate` | Delete form and validate result | `--code`, `--file`, or stdin + `--line --col` or `--index` |
| `insert-and-validate` | Insert code and validate result | `--code`, `--file`, or stdin + `--line --col --insert` or `--at-end` |
| `replace-and-validate` | Replace form and validate result | `--code`, `--file`, or stdin + `--line --col --replace` |

Optional flags: `--recovery` (error recovery), `--write` (in-place edit), `--validate` (validate inserted code), `--quiet` (suppress info output)

### Safe Editing with Validation

The `-and-validate` commands combine modification with validation:
- **Without `--write`**: Returns JSON with `source`, `balanced`, `errors`, and `warnings`
- **With `--write`**: Only writes if validation succeeds; exits with error otherwise

```bash
# Replace and validate (safe mode)
cl-toolkit replace-and-validate --file myfile.lisp --line 5 --col 2 \
  --replace "(new-form)" --write

# Insert and validate
cl-toolkit insert-and-validate --file myfile.lisp --line 1 --col 1 \
  --insert "(defun helper () ...)" --write

# Delete and validate
cl-toolkit delete-and-validate --file myfile.lisp --index 2 --write
```

## Editing Workflow

`insert` and `replace` have different semantics:

- **`insert`** adds a new top-level form at the given position. It cannot insert code inside an existing form body.
- **`replace`** replaces the form containing the given line/col, including nested subforms.

### Modifying Existing Code

To add new functionality to an existing file:

1. **Add helper functions** as new top-level forms:
   ```bash
   cl-toolkit insert --file evaluator.lisp --line 11 --col 3 \
     --insert "(defun new-helper (x) ...)" --write
   ```

2. **Replace existing forms** with modified versions:
   ```bash
   cl-toolkit replace --file evaluator.lisp --line 20 --col 3 \
     --replace "(defun dispatch-token (tok) ...)" --write
   ```

3. **Replace nested subforms** — point to any line/col inside the target:
   ```bash
   cl-toolkit replace --file evaluator.lisp --line 25 --col 5 \
     --replace "(* 5 6)" --write
   ```

4. **Pipe-based workflow** — read from stdin, write to stdout:
   ```bash
   cat evaluator.lisp | cl-toolkit format > formatted.lisp
   cat evaluator.lisp | cl-toolkit delete --index 2 > cleaned.lisp
   ```

Both `--write` commands create a `.bak` backup file.

## Requirements

- SBCL (Steel Bank Common Lisp)
- Quicklisp
- Make (for `make build`)
- Node.js (for cross-platform setup and opencode)

## Project Structure

```
cl-toolkit/
├── cl-toolkit.asd      # ASDF system definition
├── Makefile            # Build targets
├── setup.sh            # Setup script (Linux/macOS)
├── setup.ps1           # Setup script (Windows PowerShell)
├── setup.js            # Setup script (Cross-platform Node.js)
├── README.md
├── src/                # Source files
│   ├── packages.lisp
│   ├── ast.lisp
│   ├── grammar.lisp
│   ├── parser.lisp
│   └── cli.lisp
├── test/               # Tests
│   ├── cl-toolkit-tests.asd
│   └── tests.lisp
└── opencode/           # Opencode plugin
    └── tools/
        └── cl-toolkit.ts
```

## License

GPL-3.0
