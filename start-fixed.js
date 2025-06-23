#!/usr/bin/env node
import express from 'express';
import { createProxyMiddleware } from 'http-proxy-middleware';
import path from 'path';
import { fileURLToPath } from 'url';
import { spawn } from 'child_process';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

// Start Flask backend first
console.log('Starting Flask backend on port 8081...');
const backend = spawn('python', ['main_2.py'], {
  cwd: path.join(__dirname, 'backend'),
  stdio: 'inherit'
});

// Wait for backend to start
setTimeout(() => {
  console.log('Starting frontend proxy on port 5000...');
  
  const app = express();

  // Serve static files
  app.use(express.static(path.join(__dirname, 'dist')));

  // Proxy all /api requests to Flask backend
  app.use('/api', createProxyMiddleware({
    target: 'http://localhost:8081',
    changeOrigin: true,
    logLevel: 'debug'
  }));

  // Serve React app for all other routes
  app.get('*', (req, res) => {
    res.sendFile(path.join(__dirname, 'dist', 'index.html'));
  });

  app.listen(5000, '0.0.0.0', () => {
    console.log('Frontend proxy running on port 5000');
    console.log('All /api requests proxied to backend on port 8081');
  });
}, 5000);

process.on('SIGTERM', () => {
  backend.kill();
  process.exit(0);
});