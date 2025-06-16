#!/usr/bin/env node

import express from 'express';
import { spawn } from 'child_process';
import { createProxyMiddleware } from 'http-proxy-middleware';

const app = express();
const PORT = parseInt(process.env.PORT ?? "5000", 10);

app.use(express.json());
app.use(express.urlencoded({ extended: false }));

console.log('Starting Kultivate AI Production Server...');

// Start Flask backend
const flaskProcess = spawn('python', ['main_2.py'], {
  cwd: './backend',
  stdio: ['ignore', 'pipe', 'pipe']
});

flaskProcess.stdout?.on('data', (data) => {
  console.log(`Flask: ${data.toString().trim()}`);
});

flaskProcess.stderr?.on('data', (data) => {
  console.log(`Flask Error: ${data.toString().trim()}`);
});

// Health check
app.get('/health', (req, res) => {
  res.json({ status: 'ok', timestamp: new Date().toISOString() });
});

// Proxy API requests to Flask backend
app.use('/api', createProxyMiddleware({
  target: 'http://localhost:8081',
  changeOrigin: true,
  pathRewrite: { '^/api': '/api' },
  onError: (err, req, res) => {
    console.error('Proxy error:', err);
    res.status(500).json({ error: 'Backend unavailable' });
  }
}));

// Serve the main application
app.get('*', (req, res) => {
  const html = `<!DOCTYPE html>
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
            <div class="text-center max-w-2xl mx-auto p-6">
                <h1 class="text-5xl font-bold text-gray-900 mb-4">Kultivate AI</h1>
                <p class="text-xl text-gray-600 mb-8">Your AI-powered data integration platform</p>
                
                <div class="bg-white rounded-lg shadow-lg p-8">
                    <div class="mb-6">
                        <input type="text" id="chatInput" placeholder="Ask about your Keboola data..." 
                               class="w-full p-4 text-lg border border-gray-300 rounded-lg focus:outline-none focus:ring-2 focus:ring-blue-500 focus:border-transparent"/>
                    </div>
                    <button onclick="sendMessage()" 
                            class="w-full bg-blue-600 text-white p-4 text-lg rounded-lg hover:bg-blue-700 transition-colors font-semibold">
                        Ask Kultivate AI
                    </button>
                    
                    <div id="chatResponse" class="mt-6 p-4 bg-gray-50 rounded-lg hidden">
                        <div class="text-gray-700 whitespace-pre-wrap"></div>
                    </div>
                    
                    <div id="loading" class="mt-6 hidden">
                        <div class="flex items-center justify-center">
                            <div class="animate-spin rounded-full h-8 w-8 border-b-2 border-blue-600"></div>
                            <span class="ml-3 text-gray-600">Processing your request...</span>
                        </div>
                    </div>
                </div>
                
                <div class="mt-8 text-sm text-gray-500">
                    <p>Connected to your Keboola workspace with Gemini AI</p>
                </div>
            </div>
        </div>
    </div>
    
    <script>
        async function sendMessage() {
            const input = document.getElementById('chatInput');
            const response = document.getElementById('chatResponse');
            const loading = document.getElementById('loading');
            const message = input.value.trim();
            
            if (!message) return;
            
            // Show loading state
            loading.style.display = 'block';
            response.style.display = 'none';
            
            try {
                const result = await fetch('/api/chat', {
                    method: 'POST',
                    headers: { 'Content-Type': 'application/json' },
                    body: JSON.stringify({ message: message })
                });
                
                const data = await result.json();
                
                // Hide loading and show response
                loading.style.display = 'none';
                response.style.display = 'block';
                response.querySelector('div').textContent = data.response || 'No response received';
                
            } catch (error) {
                console.error('Error:', error);
                loading.style.display = 'none';
                response.style.display = 'block';
                response.querySelector('div').textContent = 'Error: Unable to connect to backend. Please try again.';
            }
            
            input.value = '';
        }
        
        // Allow Enter key to send message
        document.getElementById('chatInput').addEventListener('keypress', function(e) {
            if (e.key === 'Enter') sendMessage();
        });
        
        // Auto-focus the input
        document.getElementById('chatInput').focus();
    </script>
</body>
</html>`;

  res.setHeader('Content-Type', 'text/html');
  res.send(html);
});

// Handle graceful shutdown
process.on('SIGTERM', () => {
  console.log('Shutting down...');
  if (flaskProcess) flaskProcess.kill();
  process.exit(0);
});

process.on('SIGINT', () => {
  console.log('Shutting down...');
  if (flaskProcess) flaskProcess.kill();
  process.exit(0);
});

app.listen(PORT, "0.0.0.0", () => {
  console.log(`Kultivate AI Production Server running on port ${PORT}`);
});