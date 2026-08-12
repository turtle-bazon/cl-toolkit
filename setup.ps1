#Requires -Version 5.1
param(
    [string]$TargetDir = ".opencode"
)

$ErrorActionPreference = "Stop"
$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path

Write-Host "Setting up cl-toolkit..."

# Create opencode tools directory
$toolsDir = Join-Path $TargetDir "tools"
New-Item -ItemType Directory -Force -Path $toolsDir | Out-Null

# Copy the tool plugin
Write-Host "Installing opencode plugin..."
Copy-Item (Join-Path $ScriptDir "opencode\tools\cl-toolkit.ts") $toolsDir

# Update paths to be absolute
$pluginPath = Join-Path $toolsDir "cl-toolkit.ts"
$content = Get-Content $pluginPath -Raw
$content = $content -replace 'path\.resolve\(__dirname, "../../build/cl-toolkit"\)', "`"$($ScriptDir -replace '\\','/')\build\cl-toolkit`""
$content = $content -replace 'path\.resolve\(__dirname, "\.\./\.\."\)  // Updated by setup\.sh', "`"$($ScriptDir -replace '\\','/')`""
Set-Content $pluginPath $content

Write-Host ""
Write-Host "Setup complete!"
Write-Host "  Plugin: $pluginPath"
Write-Host "  Binary will be built automatically on first use."
