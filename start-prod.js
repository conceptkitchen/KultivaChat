#!/usr/bin/env node

import { spawn } from 'child_process';
import path from 'path';
import { fileURLToPath } from 'url';

const __dirname = path.dirname(fileURLToPath(import.meta.url));

console.log('Starting production deployment with persistent monitoring...');

let frontendProcess = null;
let backendProcess = null;

const startFrontend = () => {
  console.log('Starting frontend on port 5000...');
  frontendProcess = spawn('node', ['dist/index.js'], {
    env: { ...process.env, NODE_ENV: 'production' },
    stdio: ['inherit', 'pipe', 'pipe']
  });

  frontendProcess.stdout.on('data', (data) => {
    console.log(`[Frontend] ${data.toString().trim()}`);
  });

  frontendProcess.stderr.on('data', (data) => {
    console.error(`[Frontend Error] ${data.toString().trim()}`);
  });

  frontendProcess.on('exit', (code) => {
    console.log(`Frontend exited with code ${code}, restarting...`);
    setTimeout(startFrontend, 2000);
  });

  return frontendProcess.pid;
};

const startBackend = () => {
  console.log('Starting backend on port 8081...');
  backendProcess = spawn('python', ['main_2.py'], {
    cwd: path.join(__dirname, 'backend'),
    env: { ...process.env, PYTHONUNBUFFERED: '1', FLASK_ENV: 'production' },
    stdio: ['inherit', 'pipe', 'pipe'],
    detached: false
  });

  backendProcess.stdout.on('data', (data) => {
    const output = data.toString().trim();
    console.log(`[Backend] ${output}`);
    if (output.includes('Running on') && output.includes('8081')) {
      console.log('[Backend] Server ready - testing connection...');
      setTimeout(testBackendConnection, 2000);
    }
  });

  backendProcess.stderr.on('data', (data) => {
    console.error(`[Backend Error] ${data.toString().trim()}`);
  });

  backendProcess.on('exit', (code) => {
    console.log(`Backend exited with code ${code}, restarting in 3 seconds...`);
    setTimeout(startBackend, 3000);
  });

  return backendProcess.pid;
};

const testBackendConnection = () => {
  const http = require('http');
  const options = {
    hostname: 'localhost',
    port: 8081,
    path: '/api/health',
    method: 'GET',
    timeout: 5000
  };

  const req = http.request(options, (res) => {
    console.log(`[Backend Test] Health check: ${res.statusCode}`);
  });

  req.on('error', (err) => {
    console.log(`[Backend Test] Connection test failed: ${err.message}`);
  });

  req.on('timeout', () => {
    console.log('[Backend Test] Connection timeout');
    req.destroy();
  });

  req.end();
};

// Start both servers
const frontendPid = startFrontend();
console.log(`Frontend started with PID: ${frontendPid}`);

setTimeout(() => {
  const backendPid = startBackend();
  console.log(`Backend started with PID: ${backendPid}`);
  console.log('Both servers are running and will auto-restart if they fail');
}, 5000);

// Handle cleanup
const cleanup = () => {
  console.log('Cleaning up processes...');
  if (frontendProcess) frontendProcess.kill();
  if (backendProcess) backendProcess.kill();
  process.exit(0);
};

process.on('SIGINT', cleanup);
process.on('SIGTERM', cleanup);

// Keep the main process alive
const keepAlive = () => {
  setTimeout(keepAlive, 30000);
};
keepAlive();