# cl-toolkit

Lisp code toolkit for structural analysis and editing.

## Features

- **Parse**: Parse Lisp source code to JSON AST
- **Validate**: Check syntax and report errors/warnings
- **Find**: Find form at specific line/column
- **Extract**: Extract forms in a range
- **Top-level**: List top-level forms
- **Delete-form**: Delete form by position or index
- **Insert-form**: Insert code before a form
- **Append-form**: Insert code after a form
- **Insert**: Insert text at exact cursor position
- **Replace-form**: Replace form at position
- **Move**: Move form from one position to another
- **Balance**: Analyze parenthesis/bracket balance
- **Format**: Reformat source with consistent indentation

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
cl-toolkit delete-form --file myfile.lisp --line 5 --col 2
cl-toolkit delete-form --file myfile.lisp --index 0
echo "(defun foo ()) (defun bar ())" | cl-toolkit delete-form --index 0

# Insert code before a form
cl-toolkit insert-form --file myfile.lisp --line 5 --col 2 --insert "(new-form)"

# Insert code after a form
cl-toolkit append-form --file myfile.lisp --line 5 --col 2 --insert "(new-form)"

# Insert text at exact position
cl-toolkit insert --file myfile.lisp --line 5 --col 10 --insert " ; comment"

# Replace form
cl-toolkit replace-form --file myfile.lisp --line 5 --col 2 --replace "(replaced-form)"

# Move form
cl-toolkit move --file myfile.lisp --from-line 5 --from-col 2 --to-line 10 --to-col 2

# Check balance
cl-toolkit balance --file myfile.lisp
echo "(+ 1 2)" | cl-toolkit balance

# Format source
cl-toolkit format --file myfile.lisp
echo "(defun  foo(x)  (+ x 1))" | cl-toolkit format
```

## Commands

### Form-Level Commands

These commands work with Lisp forms (s-expressions):

| Command | Description |
|---------|-------------|
| `insert-form` | Insert code **before** a form at position |
| `append-form` | Insert code **after** a form at position |
| `replace-form` | Replace form at position with new code |
| `delete-form` | Delete form by position or index |

```bash
# insert-form: adds before the form
cl-toolkit insert-form --file demo.lisp --line 0 --col 0 --insert "(defun bar () 2)
"
# Result:
# (defun bar () 2)
# (defun foo (x) (+ x 1))

# append-form: adds after the form
cl-toolkit append-form --file demo.lisp --line 0 --col 0 --insert "
(defun baz () 3)"
# Result:
# (defun foo (x) (+ x 1))
# (defun baz () 3)
```

### Text-Level Command

| Command | Description |
|---------|-------------|
| `insert` | Insert text at exact cursor position |

```bash
# insert: adds text at exact position (no form detection)
cl-toolkit insert --file demo.lisp --line 0 --col 12 --insert " ; comment"
# Result:
# (defun foo (x) ; comment) (+ x 1))
```

### Other Commands

| Command | Description |
|---------|-------------|
| `parse` | Parse Lisp code to JSON AST |
| `validate` | Check syntax and report errors |
| `find` | Find form at position |
| `extract` | Extract forms in a range |
| `top-level` | List top-level forms |
| `move-form` | Move form from one position to another |
| `balance` | Analyze parenthesis balance |
| `format` | Reformat source with consistent indentation |

## Output Behavior

- **Read-only commands** (parse, validate, balance, etc.) output JSON to stdout
- **Modification commands** (delete-form, insert-form, append-form, replace-form, format, move) output plain text to stdout when not using `--write`
- **With `--write`**, results are written to the file (creates `.bak` backup) and a unified diff is printed to stdout
- **No changes** — if the operation produces no actual changes, "No changes made." is printed instead

## Validation

All modification commands validate both input and result by default:
- **Input validation**: Checks that the new code is valid Lisp before performing the operation
- **Result validation**: Checks that the modified source is valid after the operation

Use `--no-validate-input` and/or `--no-validate-result` to skip specific validations:

```bash
# Skip input validation only
cl-toolkit replace-form --file myfile.lisp --line 5 --col 2 \
  --replace "(unclosed" --no-validate-input

# Skip result validation only
cl-toolkit replace-form --file myfile.lisp --line 5 --col 2 \
  --replace "(unclosed" --no-validate-result

# Skip all validation (use with caution)
cl-toolkit replace-form --file myfile.lisp --line 5 --col 2 \
  --replace "(unclosed" --no-validate-input --no-validate-result
```

## Insert Behavior

### `insert-form` and `append-form`

Client controls newlines by including them in the insert code:

```bash
# Add newline before
cl-toolkit insert-form --file demo.lisp --line 0 --col 0 --insert "(defun bar () 2)
"

# Add newline after
cl-toolkit append-form --file demo.lisp --line 0 --col 0 --insert "
(defun baz () 3)"
```

### `insert`

Simple text insertion at exact position:

```bash
# Insert at column 12
cl-toolkit insert --file demo.lisp --line 0 --col 12 --insert " ; comment"
```

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
> Use cl-toolkit to parse/validate/edit Lisp code. Commands: parse, validate, find, extract, top-level, delete-form, insert-form, append-form, insert, replace-form, move, balance, format.

Example prompts:
- "Parse this file and show me the top-level forms"
- "Find the form at line 5, column 2"
- "Validate this file for syntax errors"
- "Delete the form at line 10"
- "Insert `(new-form)` before the form at line 5"
- "Append `(new-form)` after the form at line 5"
- "Move the form from line 3 to after line 8"
- "Check parenthesis balance in this file"
- "Format this file with consistent indentation"

## OpenCode Integration

When using cl-toolkit with opencode, use the **cl-toolkit plugin** (not bash) for modification commands. The plugin generates unified diffs on success.

**Correct** (returns diff):
```typescript
cl-toolkit({ command: "replace-form", filePath: "file.lisp", line: 5, col: 2, code: "(new-form)", write: true })
```

**Wrong** (returns raw JSON):
```bash
cl-toolkit replace-form -f file.lisp --line 5 --col 2 --replace "(new-form)" --write
```

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
