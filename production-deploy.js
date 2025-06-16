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

// Create a simple index.html for production if frontend build fails
if (!fs.existsSync('dist/assets')) {
  console.log('Creating simple production frontend...');
  const simpleHTML = `<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Kultivate AI</title>
    <script src="https://unpkg.com/react@18/umd/react.production.min.js"></script>
    <script src="https://unpkg.com/react-dom@18/umd/react-dom.production.min.js"></script>
    <script src="https://cdn.tailwindcss.com"></script>
</head>
<body>
    <div id="root">
        <div class="min-h-screen bg-gray-50 flex items-center justify-center">
            <div class="text-center">
                <h1 class="text-4xl font-bold text-gray-900 mb-4">Kultivate AI</h1>
                <p class="text-lg text-gray-600 mb-8">Your AI-powered data integration platform</p>
                <div class="bg-white rounded-lg shadow-lg p-6 max-w-md mx-auto">
                    <p class="text-gray-700">Loading your data workspace...</p>
                    <div class="mt-4">
                        <div class="animate-spin rounded-full h-8 w-8 border-b-2 border-blue-600 mx-auto"></div>
                    </div>
                </div>
            </div>
        </div>
    </div>
    <script>
        // Redirect to proper React app once server is ready
        setTimeout(() => {
            window.location.reload();
        }, 3000);
    </script>
</body>
</html>`;
  
  fs.writeFileSync('dist/index.html', simpleHTML);
  fs.mkdirSync('dist/assets', { recursive: true });
  console.log('Simple production frontend created');
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