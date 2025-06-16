#!/usr/bin/env node

import express from "express";
import path from "path";
import { spawn } from 'child_process';
import { fileURLToPath } from 'url';

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const app = express();
const PORT = process.env.PORT || 5000;

console.log('Kultivate AI Production Server Starting...');

// Start Flask backend
let flaskProcess;
function startFlaskServer() {
  console.log('Starting Flask backend on port 8081...');
  
  flaskProcess = spawn('python', ['main_2.py'], {
    cwd: path.join(__dirname, 'backend'),
    env: { 
      ...process.env,
      PYTHONUNBUFFERED: '1',
      FLASK_ENV: 'production'
    },
    stdio: 'inherit'
  });

  flaskProcess.on('error', (error) => {
    console.error('Flask backend error:', error);
  });
}

// Configure Express
app.use(express.json({ limit: '50mb' }));
app.use(express.urlencoded({ extended: true }));

// API proxy to Flask backend
app.use('/api', (req, res) => {
  const backendUrl = `http://localhost:8081${req.originalUrl}`;
  
  fetch(backendUrl, {
    method: req.method,
    headers: {
      'Content-Type': 'application/json',
      ...req.headers
    },
    body: req.method !== 'GET' ? JSON.stringify(req.body) : undefined
  })
  .then(response => response.json())
  .then(data => res.json(data))
  .catch(error => {
    console.error('Backend proxy error:', error);
    res.status(500).json({ error: 'Backend unavailable' });
  });
});

// Serve production frontend
app.get('*', (req, res) => {
  res.send(`
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Kultivate AI</title>
    <style>
        * { margin: 0; padding: 0; box-sizing: border-box; }
        body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif; background: #f8fafc; }
        .container { max-width: 1200px; margin: 0 auto; padding: 20px; }
        .header { text-align: center; margin-bottom: 40px; }
        .title { font-size: 2.5rem; font-weight: 700; color: #1e293b; margin-bottom: 10px; }
        .subtitle { font-size: 1.1rem; color: #64748b; }
        .status { display: inline-block; padding: 8px 16px; background: #10b981; color: white; border-radius: 20px; font-size: 0.9rem; font-weight: 500; margin: 20px 0; }
        .chat-container { background: white; border-radius: 12px; box-shadow: 0 4px 6px -1px rgba(0,0,0,0.1); padding: 24px; margin-bottom: 24px; }
        .messages { min-height: 400px; max-height: 500px; overflow-y: auto; border: 1px solid #e2e8f0; border-radius: 8px; padding: 16px; background: #fafafa; margin-bottom: 20px; }
        .message { margin: 12px 0; padding: 12px 16px; border-radius: 8px; max-width: 80%; }
        .user { background: #3b82f6; color: white; margin-left: auto; text-align: right; }
        .assistant { background: #f1f5f9; color: #1e293b; border-left: 4px solid #10b981; }
        .loading { background: #fef3c7; color: #92400e; border-left: 4px solid #f59e0b; }
        .input-container { display: flex; gap: 12px; }
        .input { flex: 1; padding: 12px 16px; border: 2px solid #e2e8f0; border-radius: 8px; font-size: 16px; transition: border-color 0.2s; }
        .input:focus { outline: none; border-color: #3b82f6; }
        .send-btn { padding: 12px 24px; background: #3b82f6; color: white; border: none; border-radius: 8px; font-weight: 500; cursor: pointer; transition: background-color 0.2s; }
        .send-btn:hover:not(:disabled) { background: #2563eb; }
        .send-btn:disabled { background: #94a3b8; cursor: not-allowed; }
        .footer { text-align: center; margin-top: 40px; color: #64748b; font-size: 0.9rem; }
    </style>
</head>
<body>
    <div class="container">
        <div class="header">
            <h1 class="title">🚀 Kultivate AI</h1>
            <p class="subtitle">AI-Powered Data Intelligence Platform</p>
            <div class="status">Production Mode - Connected</div>
        </div>

        <div class="chat-container">
            <div class="messages" id="messages"></div>
            <div class="input-container">
                <input type="text" id="messageInput" class="input" placeholder="Ask about your Keboola data..." onkeypress="handleKeyPress(event)">
                <button onclick="sendMessage()" class="send-btn" id="sendBtn">Send</button>
            </div>
        </div>

        <div class="footer">
            <p>Powered by Gemini 2.0 Flash • Connected to Keboola BigQuery Workspace</p>
        </div>
    </div>

    <script>
        let isLoading = false;
        
        async function sendMessage() {
            const input = document.getElementById('messageInput');
            const sendBtn = document.getElementById('sendBtn');
            const message = input.value.trim();
            
            if (!message || isLoading) return;
            
            isLoading = true;
            sendBtn.disabled = true;
            sendBtn.textContent = 'Sending...';
            
            addMessage('user', message);
            input.value = '';
            
            const loadingId = addMessage('assistant', '🤔 Processing your query...', 'loading');
            
            try {
                const response = await fetch('/api/messages', {
                    method: 'POST',
                    headers: { 'Content-Type': 'application/json' },
                    body: JSON.stringify({
                        conversationId: 'production-chat',
                        content: message
                    })
                });
                
                if (!response.ok) throw new Error('Network response was not ok');
                
                const data = await response.json();
                removeMessage(loadingId);
                addMessage('assistant', data.reply || data.error || 'No response received');
                
            } catch (error) {
                removeMessage(loadingId);
                addMessage('assistant', 'Connection error: ' + error.message + '. Please check if the backend is running.');
            } finally {
                isLoading = false;
                sendBtn.disabled = false;
                sendBtn.textContent = 'Send';
            }
        }
        
        function addMessage(role, content, className = '') {
            const messages = document.getElementById('messages');
            const div = document.createElement('div');
            const messageId = 'msg-' + Date.now();
            div.id = messageId;
            div.className = 'message ' + role + (className ? ' ' + className : '');
            div.textContent = content;
            messages.appendChild(div);
            messages.scrollTop = messages.scrollHeight;
            return messageId;
        }
        
        function removeMessage(messageId) {
            const element = document.getElementById(messageId);
            if (element) element.remove();
        }
        
        function handleKeyPress(event) {
            if (event.key === 'Enter' && !isLoading) {
                sendMessage();
            }
        }
        
        // Test backend connection on load
        fetch('/api/health')
            .then(response => response.ok ? 
                addMessage('assistant', '✅ Backend connection verified - Ready for data queries!') :
                addMessage('assistant', '⚠️ Backend connection issue'))
            .catch(() => addMessage('assistant', '⚠️ Backend connection failed - Starting up...'));
    </script>
</body>
</html>
  `);
});

// Start servers
startFlaskServer();

app.listen(PORT, "0.0.0.0", () => {
  console.log(`Production server running on port ${PORT}`);
  console.log('Frontend: http://localhost:5000');
  console.log('Backend: http://localhost:8081');
});

// Handle graceful shutdown
process.on('SIGTERM', () => {
  if (flaskProcess) flaskProcess.kill();
  process.exit(0);
});

process.on('SIGINT', () => {
  if (flaskProcess) flaskProcess.kill();
  process.exit(0);
});