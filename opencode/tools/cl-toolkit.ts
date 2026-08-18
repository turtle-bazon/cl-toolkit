import { tool } from "@opencode-ai/plugin"
import { execFileSync } from "child_process"
import { readFileSync, existsSync } from "fs"
import path from "path"

function generateDiff(original: string, modified: string, filePath: string): string {
  const originalLines = original.split('\n')
  const modifiedLines = modified.split('\n')
  
  while (originalLines.length > 0 && originalLines[originalLines.length - 1] === '') {
    originalLines.pop()
  }
  while (modifiedLines.length > 0 && modifiedLines[modifiedLines.length - 1] === '') {
    modifiedLines.pop()
  }
  
  if (original === modified) return ''
  
  const diff: string[] = []
  diff.push(`--- a/${filePath}`)
  diff.push(`+++ b/${filePath}`)
  
  let i = 0, j = 0
  const hunks: { startOld: number; startNew: number; lines: string[] }[] = []
  let currentHunk: { startOld: number; startNew: number; lines: string[] } | null = null
  
  while (i < originalLines.length || j < modifiedLines.length) {
    if (i < originalLines.length && j < modifiedLines.length && originalLines[i] === modifiedLines[j]) {
      if (currentHunk) {
        currentHunk.lines.push(` ${originalLines[i]}`)
      }
      i++
      j++
    } else {
      if (!currentHunk) {
        currentHunk = { startOld: i, startNew: j, lines: [] }
        hunks.push(currentHunk)
      }
      
      if (i < originalLines.length && (j >= modifiedLines.length || originalLines.indexOf(modifiedLines[j], i) === -1)) {
        currentHunk.lines.push(`-${originalLines[i]}`)
        i++
      } else if (j < modifiedLines.length) {
        currentHunk.lines.push(`+${modifiedLines[j]}`)
        j++
      }
      
      if (currentHunk && currentHunk.lines.length > 0) {
        const lastLine = currentHunk.lines[currentHunk.lines.length - 1]
        if (lastLine.startsWith(' ') && 
            i < originalLines.length && j < modifiedLines.length &&
            originalLines[i] === modifiedLines[j] &&
            originalLines[i+1] === modifiedLines[j+1] &&
            originalLines[i+2] === modifiedLines[j+2]) {
          currentHunk = null
        }
      }
    }
  }
  
  for (const hunk of hunks) {
    const oldLen = hunk.lines.filter(l => l.startsWith('-') || l.startsWith(' ')).length
    const newLen = hunk.lines.filter(l => l.startsWith('+') || l.startsWith(' ')).length
    diff.push(`@@ -${hunk.startOld + 1},${oldLen} +${hunk.startNew + 1},${newLen} @@`)
    diff.push(...hunk.lines)
  }
  
  return diff.join('\n')
}

const CL_TOOLKIT_PATH = path.resolve(__dirname, "../../build/cl-toolkit")
const REPO_DIR = path.resolve(__dirname, "../..")  // Updated by setup.sh

function ensureBinary(): void {
  if (existsSync(CL_TOOLKIT_PATH)) {
    return
  }
  throw new Error(`cl-toolkit binary not found at ${CL_TOOLKIT_PATH}. Run: cd ${REPO_DIR} && make build`)
}

interface ParseResult {
  type: string
  start: number
  end: number
  source?: string
  children?: ParseResult[]
  name?: string
  value?: number | string
  package?: string
  kind?: string
}

interface ValidateResult {
  balanced: boolean
  errors: Array<{ line: number | null; col: number | null; message: string }>
  warnings: Array<{ line: number | null; col: number | null; message: string }>
}

interface TopLevelResult {
  forms: ParseResult[]
  count: number
}

function runClToolkit(args: string[], code?: string): { stdout: string; stderr: string; exitCode: number } {
  ensureBinary()
  const input = code ? Buffer.from(code) : undefined
  try {
    const result = execFileSync(CL_TOOLKIT_PATH, args, {
      input,
      timeout: 10000,
      maxBuffer: 1024 * 1024,
      encoding: "utf-8",
    })
    return { stdout: result, stderr: "", exitCode: 0 }
  } catch (error: any) {
    // execFileSync throws on non-zero exit code
    return {
      stdout: error.stdout || "",
      stderr: error.stderr || error.message || "",
      exitCode: error.status || 1,
    }
  }
}

export default tool({
  description: "Parse, validate, and edit Common Lisp code using cl-toolkit (PEG parser with error recovery)",
  args: {
    command: tool.schema
      .enum(["parse", "validate", "find", "extract", "top-level", "delete-form", "insert-form", "append-form", "replace-form", "move-form", "insert", "balance", "format"])
      .describe("Command to execute"),
    code: tool.schema.string().optional().describe("Inline Lisp code to parse/insert/replace"),
    filePath: tool.schema.string().optional().describe("Path to .lisp file"),
    recovery: tool.schema.boolean().optional().describe("Enable error recovery (parse and modification commands)"),
    write: tool.schema.boolean().optional().describe("Write changes to file in-place (modification commands)"),
    validate: tool.schema.boolean().optional().describe("Validate new code syntax (insert --end command)"),
    noValidateInput: tool.schema.boolean().optional().describe("Skip input code validation"),
    noValidateResult: tool.schema.boolean().optional().describe("Skip result validation"),
    index: tool.schema.number().optional().describe("Top-level form index (delete --index command)"),
    line: tool.schema.number().optional().describe("Line number"),
    col: tool.schema.number().optional().describe("Column number"),
    line1: tool.schema.number().optional().describe("Start line (extract/move command)"),
    col1: tool.schema.number().optional().describe("Start column (extract/move command)"),
    line2: tool.schema.number().optional().describe("End line (extract/move command)"),
    col2: tool.schema.number().optional().describe("End column (extract/move command)"),
    indent: tool.schema.string().optional().describe("Indent string for format command (default: 2 spaces)"),
  },
  async execute(args, context) {
    const { command, code, filePath, recovery, write, validate, noValidateInput, noValidateResult, index, line, col, line1, col1, line2, col2, indent } = args

    // Resolve file path if provided
    let absolutePath: string | undefined
    if (filePath) {
      const workdir = context.worktree || process.cwd()
      absolutePath = path.resolve(workdir, filePath)
    }

    // Read original file for diff generation (modification commands only)
    let originalSource: string | undefined
    if (absolutePath && ["delete", "insert", "replace", "move", "format"].includes(command)) {
      try {
        originalSource = readFileSync(absolutePath, "utf-8")
      } catch (e) {
        // File might not exist yet
      }
    }

    // Build command arguments
    const cmdArgs: string[] = [command]

    if (command === "parse") {
      if (recovery) {
        cmdArgs.push("--recovery")
      }
      if (code) {
        cmdArgs.push("--code", code)
      } else if (absolutePath) {
        cmdArgs.push("--file", absolutePath)
      } else {
        return JSON.stringify({ error: "Provide either code or filePath for parse command" })
      }
    } else if (command === "validate") {
      if (absolutePath) {
        cmdArgs.push("--file", absolutePath)
        if (recovery) cmdArgs.push("--recovery")
      } else {
        return JSON.stringify({ error: "Provide filePath for validate command" })
      }
    } else if (command === "find") {
      if (!absolutePath || line === undefined || col === undefined) {
        return JSON.stringify({ error: "find command requires filePath, line, and col" })
      }
      cmdArgs.push("--file", absolutePath, "--line", String(line), "--col", String(col))
    } else if (command === "extract") {
      if (!absolutePath || line1 === undefined || col1 === undefined || line2 === undefined || col2 === undefined) {
        return JSON.stringify({ error: "extract command requires filePath, line1, col1, line2, col2" })
      }
      cmdArgs.push("--file", absolutePath, "--line1", String(line1), "--col1", String(col1), "--line2", String(line2), "--col2", String(col2))
    } else if (command === "top-level") {
      if (absolutePath) {
        cmdArgs.push("--file", absolutePath)
      } else {
        return JSON.stringify({ error: "Provide filePath for top-level command" })
      }
    } else if (command === "delete-form") {
      if (!absolutePath) {
        return JSON.stringify({ error: "delete-form command requires filePath" })
      }
      if (write) cmdArgs.push("--write")
      if (write) cmdArgs.push("--quiet")
      if (recovery) cmdArgs.push("--recovery")
      if (noValidateInput) cmdArgs.push("--no-validate-input")
      if (noValidateResult) cmdArgs.push("--no-validate-result")
      if (index !== undefined) {
        cmdArgs.push("--file", absolutePath, "--index", String(index))
      } else if (line !== undefined && col !== undefined) {
        cmdArgs.push("--file", absolutePath, "--line", String(line), "--col", String(col))
      } else {
        return JSON.stringify({ error: "delete-form command requires line/col or index" })
      }
    } else if (command === "insert-form") {
      if (!absolutePath) {
        return JSON.stringify({ error: "insert-form command requires filePath" })
      }
      if (write) cmdArgs.push("--write")
      if (write) cmdArgs.push("--quiet")
      if (recovery) cmdArgs.push("--recovery")
      if (noValidateInput) cmdArgs.push("--no-validate-input")
      if (noValidateResult) cmdArgs.push("--no-validate-result")
      if (line !== undefined && col !== undefined && code) {
        cmdArgs.push("--file", absolutePath, "--line", String(line), "--col", String(col), "--insert", code)
      } else {
        return JSON.stringify({ error: "insert-form command requires line, col, and code" })
      }
    } else if (command === "append-form") {
      if (!absolutePath) {
        return JSON.stringify({ error: "append-form command requires filePath" })
      }
      if (write) cmdArgs.push("--write")
      if (write) cmdArgs.push("--quiet")
      if (recovery) cmdArgs.push("--recovery")
      if (noValidateInput) cmdArgs.push("--no-validate-input")
      if (noValidateResult) cmdArgs.push("--no-validate-result")
      if (line !== undefined && col !== undefined && code) {
        cmdArgs.push("--file", absolutePath, "--line", String(line), "--col", String(col), "--insert", code)
      } else {
        return JSON.stringify({ error: "append-form command requires line, col, and code" })
      }
    } else if (command === "replace-form") {
      if (!absolutePath || line === undefined || col === undefined || !code) {
        return JSON.stringify({ error: "replace-form command requires filePath, line, col, and code" })
      }
      if (write) cmdArgs.push("--write")
      if (write) cmdArgs.push("--quiet")
      if (recovery) cmdArgs.push("--recovery")
      if (noValidateInput) cmdArgs.push("--no-validate-input")
      if (noValidateResult) cmdArgs.push("--no-validate-result")
      cmdArgs.push("--file", absolutePath, "--line", String(line), "--col", String(col), "--replace", code)
    } else if (command === "insert") {
      if (!absolutePath) {
        return JSON.stringify({ error: "insert command requires filePath" })
      }
      if (write) cmdArgs.push("--write")
      if (write) cmdArgs.push("--quiet")
      if (line !== undefined && col !== undefined && code) {
        cmdArgs.push("--file", absolutePath, "--line", String(line), "--col", String(col), "--insert", code)
      } else {
        return JSON.stringify({ error: "insert command requires line, col, and code" })
      }
    } else if (command === "move-form") {
      if (!absolutePath || line1 === undefined || col1 === undefined || line2 === undefined || col2 === undefined) {
        return JSON.stringify({ error: "move-form command requires filePath, line1, col1, line2, col2" })
      }
      if (write) cmdArgs.push("--write")
      if (write) cmdArgs.push("--quiet")
      if (recovery) cmdArgs.push("--recovery")
      cmdArgs.push("--file", absolutePath, "--from-line", String(line1), "--from-col", String(col1), "--to-line", String(line2), "--to-col", String(col2))
    } else if (command === "balance") {
      if (absolutePath) {
        cmdArgs.push("--file", absolutePath)
      } else if (code) {
        cmdArgs.push("--code", code)
      } else {
        return JSON.stringify({ error: "Provide filePath or code for balance command" })
      }
    } else if (command === "format") {
      if (absolutePath) {
        cmdArgs.push("--file", absolutePath)
        if (write) cmdArgs.push("--write")
        if (write) cmdArgs.push("--quiet")
      } else if (code) {
        cmdArgs.push("--code", code)
      } else {
        return JSON.stringify({ error: "Provide filePath or code for format command" })
      }
      if (indent) {
        cmdArgs.push("--indent", indent)
      }
    }

    try {
      const { stdout, stderr, exitCode } = runClToolkit(cmdArgs, code)

      // If cl-toolkit failed, return the error as structured JSON
      if (exitCode !== 0) {
        const rawError = stderr || stdout || "Unknown error"
        // Sanitize: strip control chars, collapse whitespace, truncate
        const sanitized = rawError
          .replace(/[\x00-\x08\x0B\x0C\x0E-\x1F]/g, "")
          .replace(/\s+/g, " ")
          .trim()
          .slice(0, 2000)
        return JSON.stringify({
          success: false,
          error: sanitized,
          _summary: `${command} failed`,
        })
      }

      // When --write is used with modification commands, CLI returns unified diff directly
      if (write && ["delete-form", "insert-form", "append-form", "replace-form", "move-form", "insert", "format"].includes(command)) {
        if (stdout.startsWith("---") && absolutePath) {
          // Count additions and deletions from diff
          const additions = (stdout.match(/^\+/gm) || []).length
          const deletions = (stdout.match(/^-/gm) || []).length
          // Return object with metadata for TUI diff rendering
          // (string return causes metadata to be dropped by the registry)
          return {
            title: `cl-toolkit ${command} ${path.basename(absolutePath)}`,
            output: stdout,
            metadata: {
              diff: stdout,
              filediff: {
                file: absolutePath,
                patch: stdout,
                additions,
                deletions,
              },
              diagnostics: {},
            },
          }
        }
        // "No changes made."
        return stdout
      }

      // Check if output is JSON before parsing (modification commands without --write return plain text)
      const isJson = stdout.trimStart().startsWith("{") || stdout.trimStart().startsWith("[")
      if (!isJson) {
        // Plain text output from modification command without --write
        if (absolutePath && ["delete-form", "insert-form", "append-form", "replace-form", "move-form", "insert"].includes(command)) {
          let diff = ""
          if (originalSource) {
            diff = generateDiff(originalSource, stdout, absolutePath)
          }
          if (diff) {
            const additions = (diff.match(/^\+/gm) || []).length
            const deletions = (diff.match(/^-/gm) || []).length
            return {
              title: `cl-toolkit ${command} ${path.basename(absolutePath)}`,
              output: diff,
              metadata: {
                diff,
                filediff: {
                  file: absolutePath,
                  patch: diff,
                  additions,
                  deletions,
                },
                diagnostics: {},
              },
            }
          }
          return JSON.stringify({
            success: true,
            source: stdout,
            _summary: `${command} command completed (no changes)`,
          })
        }
        return stdout
      }

      const result = JSON.parse(stdout)

      // Add metadata for better display
      if (command === "parse" && result.type === "LIST") {
        const formCount = result.children?.length || 0
        return JSON.stringify({
          ...result,
          _summary: `Parsed ${formCount} top-level form(s)`,
        })
      }

      if (command === "validate") {
        const errorCount = result.errors?.length || 0
        const warningCount = result.warnings?.length || 0
        return JSON.stringify({
          ...result,
          _summary: result.balanced
            ? `Balanced (${warningCount} warning(s))`
            : `Unbalanced (${errorCount} error(s), ${warningCount} warning(s))`,
        })
      }

      if (command === "top-level") {
        // top-level returns an array directly
        const forms = Array.isArray(result) ? result : result.children || []
        return JSON.stringify({
          forms,
          count: forms.length,
          _summary: `Found ${forms.length} top-level form(s)`,
        })
      }

      // Handle modification commands
      if (["delete", "insert", "replace", "move"].includes(command)) {
        // Without --write, CLI returns the modified source
        if (result.success) {
          let diff = ""
          if (originalSource && absolutePath) {
            diff = generateDiff(originalSource, result.source, absolutePath)
          }
          if (diff) {
            const additions = (diff.match(/^\+/gm) || []).length
            const deletions = (diff.match(/^-/gm) || []).length
            return {
              title: `cl-toolkit ${command} ${path.basename(absolutePath)}`,
              output: diff,
              metadata: {
                diff,
                filediff: {
                  file: absolutePath,
                  patch: diff,
                  additions,
                  deletions,
                },
                diagnostics: {},
              },
            }
          }
          return JSON.stringify({
            success: true,
            source: result.source,
            _summary: `${command} command completed successfully`,
          })
        } else {
          return JSON.stringify({
            success: false,
            error: result.error,
            _summary: `${command} command failed: ${result.error}`,
          })
        }
      }

      // Handle balance command
      if (command === "balance") {
        const finalDepth = result.final_depth || 0
        const errorCount = result.errors?.length || 0
        return JSON.stringify({
          ...result,
          _summary: finalDepth === 0
            ? `Balanced (max depth: ${result.max_depth})`
            : `Unbalanced (final depth: ${finalDepth}, ${errorCount} error(s))`,
        })
      }

      // Handle format command
      if (command === "format") {
        // Without --write, CLI returns formatted source
        let diff = ""
        if (originalSource && result.source && absolutePath) {
          diff = generateDiff(originalSource, result.source, absolutePath)
        }
        if (diff) {
          const additions = (diff.match(/^\+/gm) || []).length
          const deletions = (diff.match(/^-/gm) || []).length
          return {
            title: `cl-toolkit format ${path.basename(absolutePath)}`,
            output: diff,
            metadata: {
              diff,
              filediff: {
                file: absolutePath,
                patch: diff,
                additions,
                deletions,
              },
              diagnostics: {},
            },
          }
        }
        return JSON.stringify({
          source: result.source,
          _summary: "Code reformatted",
        })
      }

      return JSON.stringify(result)
    } catch (e: any) {
      return JSON.stringify({ error: e.message })
    }
  },
})
