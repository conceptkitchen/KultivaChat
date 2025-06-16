#!/usr/bin/env node

import { spawn } from 'child_process';

console.log('Kultivate AI Production Deployment Starting...');

// Simple production deployment that runs the built server
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