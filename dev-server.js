// Simple development proxy server to route frontend to Python backend
import express from 'express';
import { createProxyMiddleware } from 'http-proxy-middleware';
import { spawn } from 'child_process';
import path from 'path';

const app = express();
const DEV_PORT = 5000;
const BACKEND_PORT = 8080;

// Start the Python backend
console.log('Starting Python backend...');
const backend = spawn('bash', ['production-start.sh'], {
  stdio: 'inherit',
  cwd: process.cwd()
});

// Wait a moment for backend to start
setTimeout(() => {
  console.log('Setting up proxy to Python backend on port', BACKEND_PORT);
  
  // Proxy API requests to Python backend
  app.use('/api', createProxyMiddleware({
    target: `http://localhost:${BACKEND_PORT}`,
    changeOrigin: true,
    timeout: 30000,
    proxyTimeout: 30000
  }));

  // Serve static files for development
  app.use(express.static('dist'));

  // Fallback to index.html for SPA routing
  app.get('*', (req, res) => {
    res.sendFile(path.resolve('dist/index.html'));
  });

  app.listen(DEV_PORT, '0.0.0.0', () => {
    console.log(`Development server running on http://0.0.0.0:${DEV_PORT}`);
    console.log(`Proxying /api requests to backend on port ${BACKEND_PORT}`);
  });
}, 3000);

// Cleanup on exit
process.on('SIGINT', () => {
  console.log('Shutting down development server...');
  backend.kill();
  process.exit(0);
});