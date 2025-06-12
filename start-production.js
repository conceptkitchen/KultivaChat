#!/usr/bin/env node

// This script replaces the default npm start behavior to ensure both servers start
import { exec, spawn } from 'child_process';
import path from 'path';
import { fileURLToPath } from 'url';

const __dirname = path.dirname(fileURLToPath(import.meta.url));

console.log('Starting Kultivate AI production environment...');

// Kill any existing processes more effectively
const cleanup = () => {
  return new Promise((resolve) => {
    exec('pkill -f "node dist/index.js" || true', () => {
      exec('pkill -f "python main_2.py" || true', () => {
        exec('pkill -f "tsx server/index.ts" || true', () => {
          resolve();
        });
      });
    });
  });
};

await cleanup();

// Wait a moment for cleanup
setTimeout(() => {
  console.log('Starting frontend server on port 5000...');
  
  // Start the Node.js frontend
  const frontend = spawn('node', ['dist/index.js'], {
    env: { ...process.env, NODE_ENV: 'production' },
    stdio: 'pipe'
  });

  frontend.stdout.on('data', (data) => {
    console.log(`Frontend: ${data}`);
  });

  frontend.stderr.on('data', (data) => {
    console.error(`Frontend Error: ${data}`);
  });

  // Wait for frontend to start, then start backend
  setTimeout(() => {
    console.log('Starting backend server on port 8081...');
    
    const backend = spawn('python', ['main_2.py'], {
      cwd: path.join(__dirname, 'backend'),
      env: { ...process.env, PYTHONUNBUFFERED: '1' },
      stdio: 'pipe'
    });

    backend.stdout.on('data', (data) => {
      console.log(`Backend: ${data}`);
    });

    backend.stderr.on('data', (data) => {
      console.error(`Backend Error: ${data}`);
    });

    console.log('Both servers are now running!');
    console.log('Frontend: http://localhost:5000 (external port 80)');
    console.log('Backend API: http://localhost:8081');

    // Handle process termination
    const exitHandler = () => {
      console.log('Shutting down servers...');
      frontend.kill();
      backend.kill();
      process.exit(0);
    };

    process.on('SIGINT', exitHandler);
    process.on('SIGTERM', exitHandler);

    frontend.on('exit', (code) => {
      console.log(`Frontend exited with code ${code}`);
      backend.kill();
      process.exit(code);
    });

    backend.on('exit', (code) => {
      console.log(`Backend exited with code ${code}`);
      frontend.kill();
      process.exit(code);
    });

  }, 5000); // Wait 5 seconds for frontend to start

}, 2000); // Wait 2 seconds for cleanup