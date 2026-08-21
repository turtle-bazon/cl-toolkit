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

## Positioning

**All `--line` and `--col` values are 0-indexed.** Line 0 is the first line, column 0 is the first character.

```bash
# Line 0, Col 0 = first character of file
cl-toolkit find --file demo.lisp --line 0 --col 0

# Line 2, Col 1 = third line, second character
cl-toolkit find --file demo.lisp --line 2 --col 1
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
cl-toolkit top-level --file myfile.lisp --names  # Show names with indices
echo "(defun foo () (bar))" | cl-toolkit top-level

# Delete form
cl-toolkit delete-form --file myfile.lisp --line 5 --col 2
cl-toolkit delete-form --file myfile.lisp --index 0
cl-toolkit delete-form --file myfile.lisp --name bar  # Delete by name
cl-toolkit delete-form --file myfile.lisp --name bar --preview  # Show diff first
echo "(defun foo ()) (defun bar ())" | cl-toolkit delete-form --index 0

# Insert code before a form
cl-toolkit insert-form --file myfile.lisp --line 5 --col 2 --insert "(new-form)"

# Insert code after a form
cl-toolkit append-form --file myfile.lisp --line 5 --col 2 --insert "(new-form)"
cl-toolkit append-form --file myfile.lisp --end --insert "(new-form)"  # At end of file
cl-toolkit append-form --file myfile.lisp --name foo --insert "(new-form)"  # After named form

# Insert text at exact position
cl-toolkit insert --file myfile.lisp --line 5 --col 10 --insert " ; comment"

# Replace form
cl-toolkit replace-form --file myfile.lisp --line 5 --col 2 --replace "(replaced-form)"
cl-toolkit replace-form --file myfile.lisp --index 0 --replace "(replaced)" --pretty  # Preserve indentation
cl-toolkit replace-form --file myfile.lisp --name bar --replace "(defun bar () 42)"  # By name
cl-toolkit replace-form --file myfile.lisp --name bar --replace "..." --preview  # Show diff
cl-toolkit replace-form --file myfile.lisp --index 0 --replace "(replaced)" --write  # In-place

# Batch operations (multiple edits at once)
cl-toolkit batch-replace --file myfile.lisp --edits '[{"operation":"delete-index","index":1}]'
cl-toolkit batch-replace --file myfile.lisp --edits '[{"operation":"replace-index","index":0,"code":"(new-form)"}]' --write

# Move form
cl-toolkit move-form --file myfile.lisp --from-line 5 --from-col 2 --to-line 10 --to-col 2
# Note: Cannot move a form into itself (source inside dest or vice versa)

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
| `move-form` | Move form from one position to another (preserves indentation, validates no self-reference) |
| `balance` | Analyze parenthesis balance |
| `format` | Reformat source with consistent indentation |

### Move-Form Behavior

`move-form` moves a form from one position to **after** another position. The source form is deleted from its original location and inserted on a new line after the destination.

```bash
# Move cond clause 1 after clause 2
echo '(cond
  ((= x 1) "one")
  ((= x 2) "two"))' | cl-toolkit move-form --from-line 1 --from-col 2 --to-line 2 --to-col 2
# Result:
# (cond
#   ((= x 2) "two")
#   ((= x 1) "one"))
```

**Key behaviors:**
- **Indentation preserved** — moved form keeps its original indentation relative to siblings
- **Spacing matched** — blank lines between top-level forms are preserved
- **Dest promotion** — if coordinates point inside a nested expression, the dest is auto-promoted to the nearest sibling form
- **Ancestor validation** — returns error if source and dest are in a parent-child relationship

**Limitations:**
- Cannot reorder elements within a list (e.g., let bindings). Use `replace-form` instead
- Moves always insert **after** the destination. To move before a form, point to the preceding form

### Replace-Form Behavior

`replace-form` replaces the **innermost form** at the given position with new code.

```bash
# Replace a single form
echo '(defun foo () (bar))' | cl-toolkit replace-form --line 1 --col 14 --replace '(baz)'
# Result:
# (defun foo () (baz))
```

**Key behaviors:**
- **Single form matching** — matches the smallest form containing the target position
- **Position-based** — uses line/col to find the form to replace
- **Validation** — validates input and result by default

**Limitations:**

1. **Single form only** — `replace-form` replaces exactly one form at the given position. If you provide replacement code containing multiple forms, only the matched form is replaced; the rest of your replacement code is inserted in its place, but the original trailing content (closing parens, etc.) remains.

   ```bash
   # Example: trying to insert new cond clauses before the default clause
   echo '(cond
     ((= x 1) "one")
     ((= x 2) "two")
     (t stack))))' | cl-toolkit replace-form --line 4 --col 5 --replace '((string= u "MEAN") ...) (t stack))))'
   # WRONG: The tool matches only (t stack) and replaces it, leaving extra parens
   ```

2. **Use for surgical edits** — replace-form works best for replacing a single, well-bounded form with another single form. For inserting multiple forms, use `insert-form` or `append-form` instead.

3. **Large form replacement** — When replacing a large function (e.g., a `defun`), the tool may match a sub-form rather than the entire function if the coordinates point inside it. To replace an entire top-level form, use the `--index` approach or ensure coordinates point to the opening paren.

**Recommended workflows:**

- **Insert new clauses before an existing one:** Use `insert-form` to add before the target, not `replace-form`
- **Replace an entire function:** Point to the first line/column of the `defun`, or use `delete-form` + `insert-form`
- **Modify a single expression:** Use `replace-form` on the specific form

## Output Behavior

- **Read-only commands** (parse, validate, balance, etc.) output JSON to stdout
- **Modification commands** (delete-form, insert-form, append-form, replace-form, format, move-form) output plain text to stdout when not using `--write`
- **With `--write`**, results are written to the file (creates `.bak` backup) and a unified diff is printed to stdout
- **No changes** — if the operation produces no actual changes, "No changes made." is printed instead

## Validation

All modification commands validate both input and result by default:
- **Input validation**: Checks that the new code is valid Lisp before performing the operation
- **Result validation**: Checks that the modified source is valid after the operation

**Note:** The PEG parser handles standard Lisp syntax including backquote, reader macros, and special forms. If you encounter a validation failure, check that your code has balanced parentheses and correct syntax. Use `--no-validate-input` or `--no-validate-result` only as a last resort when you're certain the code is valid but the parser rejects it.

## Insert Behavior

### `insert-form` and `append-form`

Client controls newlines by including them in the insert code. This is intentional — the tool inserts code as-is without adding automatic newlines, giving the caller full control over formatting:

```bash
# Add newline before
cl-toolkit insert-form --file demo.lisp --line 0 --col 0 --insert "(defun bar () 2)
"

# Add newline after
cl-toolkit append-form --file demo.lisp --line 0 --col 0 --insert "
(defun baz () 3)"
```

Without leading/trailing newlines in `--insert`, the code is inserted inline:

```bash
# Inline insertion (no newlines)
cl-toolkit insert-form --file demo.lisp --line 0 --col 12 --insert " ; comment"
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
> Use cl-toolkit to parse/validate/edit Lisp code. Commands: parse, validate, find, extract, top-level, source-of, find-forms, delete-form, insert-form, append-form, insert, replace-form, move-form, balance, format.

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

### Safe Read-Modify-Write of Large Forms

Never reconstruct a large function from memory. Extract exact current
source first:

```bash
# 1. Read exact source
cl-toolkit source-of --file X.lisp --name dispatch-token > /tmp/form.lisp

# 2. Edit /tmp/form.lisp (any tool)

# 3. Write back by name
cl-toolkit replace-form --file X.lisp --name dispatch-token \
  --replace "$(cat /tmp/form.lisp)" --write
```

### Surgical Subform Replacement

Change one clause inside a large function without rewriting it:

```bash
# Replace smallest subform matching the snippet inside the named form
cl-toolkit replace-form --file X.lisp --name dispatch-token \
  --match '(string= tok "?")' --replace '(string= tok "?" :test #'equal)'
```

Exact trimmed matches are preferred; falls back to first contains-match.
Every non-quiet edit reports which form was targeted with its resolved
line/col — verify that output before trusting a write.

### Structural Search

```bash
# Find every top-level form containing a snippet (catches duplicated bugs)
cl-toolkit find-forms --file src/*.lisp --contains "(nreverse args)" --with-source
```

### Line Numbers Are 0-Based

All `--line` arguments and displayed `[line N]` values use 0-based,
newline-delimited counting. A value shown anywhere can be passed back
to `--line` verbatim.

### Shell Quoting

Arguments containing `#'` and quoted lists are fragile in bash. Prefer
the reader-macro-free spellings — they are semantically identical:

```bash
# Fragile:
--replace "(member s '(:a) :test #'+)"
# Robust:
--replace "(member s (quote (:a)) :test (function +))"
```

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
