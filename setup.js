#!/usr/bin/env node

const fs = require('fs');
const path = require('path');

const scriptDir = __dirname;
const targetDir = process.argv[2] || '.opencode';

console.log('Setting up cl-toolkit...');

// Create opencode tools directory
const toolsDir = path.join(targetDir, 'tools');
fs.mkdirSync(toolsDir, { recursive: true });

// Create plugins directory
const pluginsDir = path.join(targetDir, 'plugins');
fs.mkdirSync(pluginsDir, { recursive: true });

// Copy the tool plugin
console.log('Installing opencode plugin...');
const pluginSrc = path.join(scriptDir, 'opencode', 'tools', 'cl-toolkit.ts');
const pluginDst = path.join(toolsDir, 'cl-toolkit.ts');
fs.copyFileSync(pluginSrc, pluginDst);

// Copy the diff viewer plugin
const diffPluginSrc = path.join(scriptDir, 'opencode', 'plugins', 'diff-viewer.js');
const diffPluginDst = path.join(pluginsDir, 'diff-viewer.js');
fs.copyFileSync(diffPluginSrc, diffPluginDst);

// Update paths to be absolute
let content = fs.readFileSync(pluginDst, 'utf-8');
const escapedDir = scriptDir.replace(/\\/g, '\\\\');
content = content.replace(
    /path\.resolve\(__dirname, "\.\.\/\.\.\/build\/cl-toolkit"\)/g,
    `"${escapedDir}/build/cl-toolkit"`
);
content = content.replace(
    /path\.resolve\(__dirname, "\.\.\/\.\.\"\)  \/\/ Updated by setup\.sh/g,
    `"${escapedDir}"`
);
fs.writeFileSync(pluginDst, content);

console.log('');
console.log('Setup complete!');
console.log(`  Plugin: ${pluginDst}`);
console.log(`  Diff viewer: ${diffPluginDst}`);
console.log('  Binary will be built automatically on first use.');
