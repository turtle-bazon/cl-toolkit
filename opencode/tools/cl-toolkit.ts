import { tool } from "@opencode-ai/plugin"
import { execFileSync } from "child_process"
import { readFileSync, existsSync } from "fs"
import { createTwoFilesPatch } from "diff"
import path from "path"

function generateDiff(original: string, modified: string, filePath: string): string {
  if (original === modified) return ''
  return createTwoFilesPatch(
    `a/${filePath}`,
    `b/${filePath}`,
    original,
    modified,
    '',
    '',
    { context: 3 }
  )
}

const CL_TOOLKIT_PATH = path.resolve(__dirname, "../../build/cl-toolkit")
const REPO_DIR = path.resolve(__dirname, "../..")  // Updated by setup.sh

function ensureBinary(): void {
  if (existsSync(CL_TOOLKIT_PATH)) {
    return
  }
  console.log("cl-toolkit binary not found, building...")
  execFileSync("make", ["build"], {
    cwd: REPO_DIR,
    timeout: 120000,
    stdio: "inherit",
  })
  if (!existsSync(CL_TOOLKIT_PATH)) {
    throw new Error(`Failed to build cl-toolkit binary at ${CL_TOOLKIT_PATH}`)
  }
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

function runClToolkit(args: string[], code?: string): string {
  ensureBinary()
  try {
    const input = code ? Buffer.from(code) : undefined
    const result = execFileSync(CL_TOOLKIT_PATH, args, {
      input,
      timeout: 10000,
      maxBuffer: 1024 * 1024,
      encoding: "utf-8",
    })
    return result
  } catch (e: any) {
    throw new Error(`cl-toolkit failed: ${e.stderr || e.message}`)
  }
}

export default tool({
  description: "Parse, validate, and edit Common Lisp code using cl-toolkit (PEG parser with error recovery)",
  args: {
    command: tool.schema
      .enum(["parse", "validate", "find", "extract", "top-level", "delete", "insert", "replace", "move", "balance", "format"])
      .describe("Command to execute"),
    code: tool.schema.string().optional().describe("Inline Lisp code to parse/insert/replace"),
    filePath: tool.schema.string().optional().describe("Path to .lisp file"),
    recovery: tool.schema.boolean().optional().describe("Enable error recovery (parse and modification commands)"),
    write: tool.schema.boolean().optional().describe("Write changes to file in-place (modification commands)"),
    validate: tool.schema.boolean().optional().describe("Validate new code syntax (insert --end command)"),
    noValidateInput: tool.schema.boolean().optional().describe("Skip input code validation"),
    noValidateResult: tool.schema.boolean().optional().describe("Skip result validation"),
    after: tool.schema.boolean().optional().describe("Insert after the form instead of before (insert command)"),
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
    const { command, code, filePath, recovery, write, after, validate, noValidateInput, noValidateResult, index, line, col, line1, col1, line2, col2, indent } = args

    // Resolve file path if provided
    let absolutePath: string | undefined
    if (filePath) {
      absolutePath = path.resolve(context.worktree, filePath)
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
    } else if (command === "delete") {
      if (!absolutePath) {
        return JSON.stringify({ error: "delete command requires filePath" })
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
        return JSON.stringify({ error: "delete command requires line/col or index" })
      }
    } else if (command === "insert") {
      if (!absolutePath) {
        return JSON.stringify({ error: "insert command requires filePath" })
      }
      if (write) cmdArgs.push("--write")
      if (write) cmdArgs.push("--quiet")
      if (recovery) cmdArgs.push("--recovery")
      if (validate) cmdArgs.push("--validate")
      if (noValidateInput) cmdArgs.push("--no-validate-input")
      if (noValidateResult) cmdArgs.push("--no-validate-result")
      if (after) cmdArgs.push("--after")
      if (line !== undefined && col !== undefined && code) {
        cmdArgs.push("--file", absolutePath, "--line", String(line), "--col", String(col), "--insert", code)
      } else {
        return JSON.stringify({ error: "insert command requires line, col, and code" })
      }
    } else if (command === "replace") {
      if (!absolutePath || line === undefined || col === undefined || !code) {
        return JSON.stringify({ error: "replace command requires filePath, line, col, and code" })
      }
      if (write) cmdArgs.push("--write")
      if (write) cmdArgs.push("--quiet")
      if (recovery) cmdArgs.push("--recovery")
      if (noValidateInput) cmdArgs.push("--no-validate-input")
      if (noValidateResult) cmdArgs.push("--no-validate-result")
      cmdArgs.push("--file", absolutePath, "--line", String(line), "--col", String(col), "--replace", code)
    } else if (command === "move") {
      if (!absolutePath || line1 === undefined || col1 === undefined || line2 === undefined || col2 === undefined) {
        return JSON.stringify({ error: "move command requires filePath, line1, col1, line2, col2" })
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
      const output = runClToolkit(cmdArgs, code)
      const result = JSON.parse(output)

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
        if (result.success) {
          let diff = ""
          if (originalSource) {
            diff = generateDiff(originalSource, result.source, absolutePath || "file")
          }
          // Return diff on success, JSON on error
          return diff || JSON.stringify({
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
        let diff = ""
        if (originalSource && result.source) {
          diff = generateDiff(originalSource, result.source, absolutePath || "file")
        }
        // Return diff on success, JSON on error
        return diff || JSON.stringify({
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
