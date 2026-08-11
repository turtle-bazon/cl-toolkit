export const DiffViewerPlugin = async ({ project, client, $, directory, worktree }) => {
  return {
    "tool.execute.after": async (input, output) => {
      if (input.tool === "cl-toolkit") {
        const args = input.args || {}
        const isModification = ["delete", "insert", "replace", "move", "format"].includes(args.command)
        
        if (isModification && output.result && !output.result.startsWith("{")) {
          return {
            ...output,
            filePath: args.filePath,
            patchText: output.result,
          }
        }
      }
      return output
    }
  }
}
