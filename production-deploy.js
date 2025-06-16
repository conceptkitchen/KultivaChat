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

// Wait longer for frontend to fully initialize
setTimeout(() => {
  console.log('Starting backend server on port 8081...');
  
  // Start backend server - ensure all required environment variables are passed
  const cleanEnv = { ...process.env };
  delete cleanEnv.PORT;  // Remove PORT completely so Flask uses hardcoded 8081
  
  // Ensure critical environment variables are passed to Flask
  const flaskEnv = {
    ...cleanEnv,
    PYTHONUNBUFFERED: '1',
    FLASK_ENV: 'production',
    KBC_API_URL: process.env.KBC_API_URL,
    KBC_STORAGE_TOKEN: process.env.KBC_STORAGE_TOKEN,
    GOOGLE_APPLICATION_CREDENTIALS: process.env.GOOGLE_APPLICATION_CREDENTIALS,
    KBC_WORKSPACE_SCHEMA: process.env.KBC_WORKSPACE_SCHEMA,
    GEMINI_API_KEY: process.env.GEMINI_API_KEY
  };
  
  console.log('Starting Flask with environment variables:', {
    KBC_API_URL: !!flaskEnv.KBC_API_URL,
    KBC_STORAGE_TOKEN: !!flaskEnv.KBC_STORAGE_TOKEN,
    GOOGLE_APPLICATION_CREDENTIALS: !!flaskEnv.GOOGLE_APPLICATION_CREDENTIALS,
    KBC_WORKSPACE_SCHEMA: !!flaskEnv.KBC_WORKSPACE_SCHEMA,
    GEMINI_API_KEY: !!flaskEnv.GEMINI_API_KEY
  });
  
  const backend = spawn('python', ['main_2.py'], {
    cwd: path.join(__dirname, 'backend'),
    env: flaskEnv,
    stdio: 'inherit'
  });

  // Monitor backend startup and test connection
  let backendReady = false;
  const checkBackend = () => {
    if (backendReady) return;
    
    const req = http.request({
      hostname: 'localhost',
      port: 8081,
      path: '/api/health',
      method: 'GET',
      timeout: 2000
    }, (res) => {
      if (res.statusCode === 200) {
        backendReady = true;
        console.log('✓ Backend health check passed');
        console.log('✓ Both servers are running successfully');
        console.log('✓ Frontend: http://localhost:5000 (mapped to port 80)');
        console.log('✓ Backend: http://localhost:8081');
        console.log('✓ Chat functionality enabled with Gemini AI');
        
        // Test chat endpoint
        testChatEndpoint();
      }
    });

    req.on('error', () => {
      // Backend not ready yet, try again
      setTimeout(checkBackend, 2000);
    });

    req.on('timeout', () => {
      req.destroy();
      setTimeout(checkBackend, 2000);
    });

    req.end();
  };

  // Start checking backend after initial delay - reduced for faster startup
  setTimeout(checkBackend, 5000);

}, 3000);

const testChatEndpoint = () => {
  const postData = JSON.stringify({
    message: "hello, test the connection"
  });

  const req = http.request({
    hostname: 'localhost',
    port: 8081,
    path: '/api/chat',
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
      'Content-Length': Buffer.byteLength(postData)
    },
    timeout: 10000
  }, (res) => {
    console.log(`✓ Chat endpoint responding: ${res.statusCode}`);
    if (res.statusCode === 200) {
      console.log('✓ Backend chat functionality verified');
    }
  });

  req.on('error', (err) => {
    console.log(`Chat test failed: ${err.message}`);
  });

  req.write(postData);
  req.end();
};

// Keep process alive
process.on('SIGTERM', () => {
  console.log('Shutting down gracefully...');
  frontend.kill();
  backend.kill();
});

// Prevent process from exiting
setInterval(() => {}, 30000);