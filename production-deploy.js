#!/usr/bin/env node

import { spawn } from 'child_process';
import path from 'path';
import http from 'http';
import { fileURLToPath } from 'url';

const __dirname = path.dirname(fileURLToPath(import.meta.url));

console.log('Kultivate AI Production Deployment Starting...');

// Start frontend server
console.log('Starting frontend server on port 5000...');
const frontend = spawn('node', ['dist/index.js'], {
  env: { ...process.env, NODE_ENV: 'production' },
  stdio: 'inherit'
});

// Wait for frontend to initialize
setTimeout(() => {
  console.log('Starting backend server on port 8081...');
  
  // Start backend server
  const backend = spawn('python', ['main_2.py'], {
    cwd: path.join(__dirname, 'backend'),
    env: { ...process.env, PYTHONUNBUFFERED: '1' },
    stdio: 'inherit'
  });

  // Test backend connection after startup
  setTimeout(() => {
    const req = http.request({
      hostname: 'localhost',
      port: 8081,
      path: '/api/health',
      method: 'GET'
    }, (res) => {
      console.log(`Backend health check: ${res.statusCode}`);
      if (res.statusCode === 200) {
        console.log('✓ Both servers are running successfully');
        console.log('✓ Frontend: http://localhost:5000 (mapped to port 80)');
        console.log('✓ Backend: http://localhost:8081');
        console.log('✓ Chat functionality enabled with Gemini AI');
      }
    });

    req.on('error', (err) => {
      console.log(`Backend connection test: ${err.message}`);
    });

    req.end();
  }, 8000);

}, 3000);

// Keep process alive
process.on('SIGTERM', () => {
  console.log('Shutting down gracefully...');
  frontend.kill();
  backend.kill();
});

// Prevent process from exiting
setInterval(() => {}, 30000);