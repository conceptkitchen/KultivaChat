#!/usr/bin/env node

import { spawn } from 'child_process';
import { execSync } from 'child_process';
import fs from 'fs';

console.log('Kultivate AI Production Deployment Starting...');

// Build server component
console.log('Building server...');
try {
  execSync('npx esbuild server/index.ts --platform=node --packages=external --bundle --format=esm --outdir=dist', { stdio: 'inherit' });
  console.log('Server build completed');
} catch (error) {
  console.error('Server build failed:', error);
  process.exit(1);
}

// Ensure dist directory exists
if (!fs.existsSync('dist')) {
  fs.mkdirSync('dist', { recursive: true });
}

// Start the production server
console.log('Starting production server...');
const server = spawn('node', ['dist/index.js'], {
  env: { ...process.env, NODE_ENV: 'production' },
  stdio: 'inherit'
});

server.on('error', (error) => {
  console.error('Server failed to start:', error);
  process.exit(1);
});

server.on('close', (code) => {
  console.log(`Server process exited with code ${code}`);
  process.exit(code);
});

// Handle graceful shutdown
process.on('SIGTERM', () => {
  console.log('Shutting down...');
  server.kill();
});

process.on('SIGINT', () => {
  console.log('Shutting down...');
  server.kill();
});