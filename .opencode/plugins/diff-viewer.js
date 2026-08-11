export default function CustomDiffPlugin({ on }) {
  on("tool.execute.after", async (input, output) => {
    if (input.tool === "cl-toolkit") {
      const args = input.args || {}
      const isModification = ["delete", "insert", "replace", "move", "format"].includes(args.command)
      
      if (isModification && output.result && !output.result.startsWith("{")) {
        // Result is a diff (starts with ---), not JSON
        return {
          ...output,
          patchText: output.result,
        }
      }
    }
    return output
  })
}
