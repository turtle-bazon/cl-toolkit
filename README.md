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

## Build

```bash
cd lisp
make build
```

Produces `lisp/build/cl-toolkit`.

## Usage

```bash
# Parse a file
cl-toolkit parse --file myfile.lisp

# Parse inline code
cl-toolkit parse --code "(+ 1 2)"

# Validate a file
cl-toolkit validate --file myfile.lisp

# Find form at position
cl-toolkit find --file myfile.lisp --line 5 --col 2

# List top-level forms
cl-toolkit top-level --file myfile.lisp

# Delete form at position
cl-toolkit delete --file myfile.lisp --line 5 --col 2

# Insert code
cl-toolkit insert --file myfile.lisp --line 5 --col 2 --code "(new-form)"

# Replace form
cl-toolkit replace --file myfile.lisp --line 5 --col 2 --code "(replaced-form)"

# Move form
cl-toolkit move --file myfile.lisp --from-line 5 --from-col 2 --to-line 10 --to-col 2

# Check balance
cl-toolkit balance --file myfile.lisp

# Format source
cl-toolkit format --file myfile.lisp
```

## Global Setup

```bash
# 1. Clone
git clone git@github.com:turtle-bazon/cl-toolkit.git ~/cl-toolkit

# 2. Install opencode plugin
mkdir -p ~/.config/opencode/tools
cp ~/cl-toolkit/opencode/tools/cl-toolkit.ts ~/.config/opencode/tools/

# 3. Update plugin to use repo path
sed -i 's|path.resolve(__dirname, "../../lisp/build/cl-toolkit")|path.resolve(process.env.HOME, "cl-toolkit/lisp/build/cl-toolkit")|' ~/.config/opencode/tools/cl-toolkit.ts

# 4. Add to opencode config
mkdir -p ~/.config/opencode
cat > ~/.config/opencode/opencode.json << 'EOF'
{
  "tools": {
    "cl-toolkit": {
      "path": "~/.config/opencode/tools/cl-toolkit.ts"
    }
  }
}
EOF
```

Binary is built automatically on first use.

### Per-Project Setup

```bash
cd /path/to/your/project
~/cl-toolkit/setup.sh .opencode
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

| Command | Description | Required Args |
|---------|-------------|---------------|
| `parse` | Parse Lisp code to JSON AST | `--file` or `--code` |
| `validate` | Check syntax and report errors | `--file` |
| `find` | Find form at position | `--file --line --col` |
| `extract` | Extract forms in a range | `--file --line1 --col1 --line2 --col2` |
| `top-level` | List top-level forms | `--file` |
| `delete` | Delete form by position or index | `--file --line --col` or `--file --index` |
| `insert` | Insert code before/after a form | `--file --line --col --code` |
| `replace` | Replace form at position | `--file --line --col --code` |
| `move` | Move form from one position to another | `--file --from-line --from-col --to-line --to-col` |
| `balance` | Analyze parenthesis balance | `--file` or `--code` |
| `format` | Reformat source with consistent indentation | `--file` or `--code` |

Optional flags: `--recovery` (error recovery), `--write` (in-place edit), `--validate` (validate inserted code)

## License

GPL-3.0
