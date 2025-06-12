#!/usr/bin/env node

const { spawn } = require('child_process');
const path = require('path');

console.log('Starting Kultivate AI production servers...');

// Kill any existing processes
const killProcesses = () => {
  try {
    spawn('pkill', ['-f', 'node dist/index.js'], { stdio: 'ignore' });
    spawn('pkill', ['-f', 'python main_2.py'], { stdio: 'ignore' });
    spawn('pkill', ['-f', 'tsx server/index.ts'], { stdio: 'ignore' });
  } catch (e) {
    // Ignore errors
  }
};

const sleep = (ms) => new Promise(resolve => setTimeout(resolve, ms));

const startServers = async () => {
  killProcesses();
  await sleep(2000);

  console.log('Starting Node.js frontend on port 5000...');
  
  // Start frontend
  const frontend = spawn('node', ['dist/index.js'], {
    env: { ...process.env, NODE_ENV: 'production' },
    stdio: 'inherit'
  });

  console.log(`Frontend started with PID: ${frontend.pid}`);
  
  // Wait for frontend to initialize
  await sleep(5000);
  
  console.log('Starting Python Flask backend on port 8081...');
  
  // Start backend
  const backend = spawn('python', ['main_2.py'], {
    cwd: path.join(__dirname, 'backend'),
    env: { ...process.env, PYTHONUNBUFFERED: '1' },
    stdio: 'inherit'
  });

  console.log(`Backend started with PID: ${backend.pid}`);
  
  // Handle cleanup
  const cleanup = () => {
    console.log('Shutting down servers...');
    frontend.kill();
    backend.kill();
    process.exit(0);
  };

  process.on('SIGINT', cleanup);
  process.on('SIGTERM', cleanup);

  // Wait for processes
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

  console.log('Both servers are running!');
  console.log('Frontend: http://localhost:5000 (external port 80)');
  console.log('Backend API: http://localhost:8081');
};

startServers().catch(console.error);