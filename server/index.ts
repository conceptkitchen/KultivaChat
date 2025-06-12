import express from "express";
import path from "path";
import { fileURLToPath } from 'url';
import { registerRoutes } from "./routes";

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

const app = express();
app.use(express.json());
app.use(express.urlencoded({ extended: false }));

const PORT = parseInt(process.env.PORT ?? "5000", 10);

// Health check
app.get('/health', (req, res) => {
  res.json({ status: 'ok', timestamp: new Date().toISOString() });
});

// Setup authentication and routes
async function initializeServer() {
  try {
    // Register API routes with authentication
    await registerRoutes(app);
    
    // Serve login page
    app.get('*', (req, res) => {
      res.send(`<!DOCTYPE html>
<html>
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Kultivate AI</title>
    <style>
        * { margin: 0; padding: 0; box-sizing: border-box; }
        body { 
            font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
            background: #f8fafc;
            min-height: 100vh;
            display: flex;
            align-items: center;
            justify-content: center;
        }
        .container {
            background: white;
            padding: 2rem;
            border-radius: 12px;
            box-shadow: 0 4px 6px rgba(0,0,0,0.05);
            width: 100%;
            max-width: 400px;
            text-align: center;
            border: 1px solid #e2e8f0;
        }
        .logo {
            width: 60px;
            height: 60px;
            background: #10b981;
            border-radius: 12px;
            margin: 0 auto 1rem;
            display: flex;
            align-items: center;
            justify-content: center;
            font-size: 24px;
            font-weight: bold;
            color: white;
        }
        h1 { 
            font-size: 1.8rem; 
            margin-bottom: 0.5rem; 
            color: #1f2937;
        }
        .highlight { color: #10b981; }
        p { 
            color: #6b7280; 
            margin-bottom: 2rem; 
            line-height: 1.5;
        }
        .tabs {
            display: flex;
            margin-bottom: 1rem;
            border-radius: 8px;
            background: #f3f4f6;
            padding: 4px;
        }
        .tab {
            flex: 1;
            padding: 0.75rem;
            border: none;
            background: transparent;
            cursor: pointer;
            border-radius: 6px;
            font-weight: 500;
            transition: all 0.2s;
        }
        .tab.active {
            background: white;
            box-shadow: 0 2px 4px rgba(0,0,0,0.1);
            color: #10b981;
        }
        .form {
            display: none;
        }
        .form.active {
            display: block;
        }
        .input-group {
            margin-bottom: 1rem;
            text-align: left;
        }
        label {
            display: block;
            margin-bottom: 0.5rem;
            font-weight: 500;
            color: #374151;
        }
        input {
            width: 100%;
            padding: 0.75rem;
            border: 2px solid #e5e7eb;
            border-radius: 8px;
            font-size: 16px;
            transition: border-color 0.2s;
        }
        input:focus {
            outline: none;
            border-color: #10b981;
        }
        .btn {
            width: 100%;
            padding: 0.875rem;
            background: #10b981;
            color: white;
            border: none;
            border-radius: 8px;
            font-size: 16px;
            font-weight: 600;
            cursor: pointer;
            transition: all 0.2s;
            margin-top: 1rem;
        }
        .btn:hover {
            background: #059669;
            transform: translateY(-1px);
        }
        .btn:disabled {
            opacity: 0.6;
            cursor: not-allowed;
            transform: none;
        }
        .message {
            margin-top: 1rem;
            padding: 0.75rem;
            border-radius: 6px;
            font-size: 14px;
            text-align: center;
        }
        .error {
            background: #fef2f2;
            color: #dc2626;
            border: 1px solid #fecaca;
        }
        .success {
            background: #f0fdf4;
            color: #16a34a;
            border: 1px solid #bbf7d0;
        }
        .grid {
            display: grid;
            grid-template-columns: 1fr 1fr;
            gap: 1rem;
        }
        .chat-container {
            display: flex;
            height: 100vh;
            background: #f9fafb;
        }
        .chat-main {
            flex: 1;
            display: flex;
            flex-direction: column;
        }
        .chat-header {
            background: white;
            border-bottom: 1px solid #e5e7eb;
            padding: 1rem;
            display: flex;
            justify-content: space-between;
            align-items: center;
        }
        .chat-content {
            flex: 1;
            display: flex;
            flex-direction: column;
            padding: 1rem;
        }
        .messages {
            flex: 1;
            background: white;
            border-radius: 8px;
            padding: 1rem;
            margin-bottom: 1rem;
            overflow-y: auto;
        }
        .input-area {
            display: flex;
            gap: 0.5rem;
        }
        .message-input {
            flex: 1;
            padding: 0.75rem;
            border: 1px solid #d1d5db;
            border-radius: 6px;
            font-size: 16px;
        }
        .send-btn {
            padding: 0.75rem 1rem;
            background: #10b981;
            color: white;
            border: none;
            border-radius: 6px;
            cursor: pointer;
            font-weight: 500;
        }
        .user-message {
            margin-bottom: 1rem;
            text-align: right;
        }
        .user-bubble {
            display: inline-block;
            background: #10b981;
            color: white;
            padding: 0.75rem;
            border-radius: 8px;
            max-width: 70%;
        }
        .ai-message {
            margin-bottom: 1rem;
        }
        .ai-bubble {
            display: inline-block;
            background: #f3f4f6;
            padding: 0.75rem;
            border-radius: 8px;
            max-width: 70%;
        }
        .loading {
            color: #6b7280;
        }
        .error-bubble {
            background: #fef2f2;
            color: #dc2626;
            border: 1px solid #fecaca;
        }
    </style>
</head>
<body>
    <div id="app">
        <div class="container">
            <div class="logo">KA</div>
            <h1>Kultivate <span class="highlight">AI</span></h1>
            <p>Sign in to access your AI-powered data insights</p>
            
            <div class="tabs">
                <button class="tab active" onclick="showLogin()">Login</button>
                <button class="tab" onclick="showRegister()">Register</button>
            </div>
            
            <form id="loginForm" class="form active" onsubmit="handleLogin(event)">
                <div class="input-group">
                    <label>Username</label>
                    <input type="text" id="loginUsername" required>
                </div>
                <div class="input-group">
                    <label>Password</label>
                    <input type="password" id="loginPassword" required>
                </div>
                <button type="submit" class="btn">Sign In</button>
            </form>
            
            <form id="registerForm" class="form" onsubmit="handleRegister(event)">
                <div class="input-group">
                    <label>Username</label>
                    <input type="text" id="regUsername" required>
                </div>
                <div class="input-group">
                    <label>Password</label>
                    <input type="password" id="regPassword" required>
                </div>
                <div class="input-group">
                    <label>Email (optional)</label>
                    <input type="email" id="regEmail">
                </div>
                <div class="grid">
                    <div class="input-group">
                        <label>First Name</label>
                        <input type="text" id="regFirstName">
                    </div>
                    <div class="input-group">
                        <label>Last Name</label>
                        <input type="text" id="regLastName">
                    </div>
                </div>
                <button type="submit" class="btn">Create Account</button>
            </form>
            
            <div id="message"></div>
        </div>
    </div>

    <script>
        let currentUser = null;
        
        function showLogin() {
            document.querySelectorAll('.tab').forEach(t => t.classList.remove('active'));
            document.querySelectorAll('.form').forEach(f => f.classList.remove('active'));
            event.target.classList.add('active');
            document.getElementById('loginForm').classList.add('active');
            clearMessage();
        }
        
        function showRegister() {
            document.querySelectorAll('.tab').forEach(t => t.classList.remove('active'));
            document.querySelectorAll('.form').forEach(f => f.classList.remove('active'));
            event.target.classList.add('active');
            document.getElementById('registerForm').classList.add('active');
            clearMessage();
        }
        
        function showMessage(text, isError = false) {
            const messageDiv = document.getElementById('message');
            messageDiv.textContent = text;
            messageDiv.className = 'message ' + (isError ? 'error' : 'success');
        }
        
        function clearMessage() {
            document.getElementById('message').textContent = '';
            document.getElementById('message').className = '';
        }
        
        async function handleLogin(event) {
            event.preventDefault();
            const username = document.getElementById('loginUsername').value;
            const password = document.getElementById('loginPassword').value;
            
            try {
                const response = await fetch('/api/login', {
                    method: 'POST',
                    headers: { 'Content-Type': 'application/json' },
                    body: JSON.stringify({ username, password })
                });
                
                if (response.ok) {
                    const user = await response.json();
                    showMessage('Login successful!');
                    setTimeout(() => showChatInterface(user), 1000);
                } else {
                    const error = await response.text();
                    showMessage('Login failed: ' + error, true);
                }
            } catch (error) {
                showMessage('Login failed: ' + error.message, true);
            }
        }
        
        async function handleRegister(event) {
            event.preventDefault();
            const username = document.getElementById('regUsername').value;
            const password = document.getElementById('regPassword').value;
            const email = document.getElementById('regEmail').value;
            const firstName = document.getElementById('regFirstName').value;
            const lastName = document.getElementById('regLastName').value;
            
            try {
                const response = await fetch('/api/register', {
                    method: 'POST',
                    headers: { 'Content-Type': 'application/json' },
                    body: JSON.stringify({ username, password, email, firstName, lastName })
                });
                
                if (response.ok) {
                    const user = await response.json();
                    showMessage('Registration successful!');
                    setTimeout(() => showChatInterface(user), 1000);
                } else {
                    const error = await response.text();
                    showMessage('Registration failed: ' + error, true);
                }
            } catch (error) {
                showMessage('Registration failed: ' + error.message, true);
            }
        }
        
        function showChatInterface(user) {
            currentUser = user;
            document.getElementById('app').innerHTML = \`
                <div class="chat-container">
                    <div class="chat-main">
                        <div class="chat-header">
                            <div style="display: flex; align-items: center;">
                                <div style="width: 32px; height: 32px; background: #10b981; border-radius: 6px; display: flex; align-items: center; justify-content: center; color: white; font-weight: bold; margin-right: 0.5rem;">KA</div>
                                <h1 style="font-size: 1.25rem; font-weight: 600;">Kultivate <span style="color: #10b981;">AI</span></h1>
                            </div>
                            <div style="display: flex; align-items: center; gap: 1rem;">
                                <span style="color: #6b7280;">Welcome, \${user.username}</span>
                                <button onclick="logout()" style="padding: 0.5rem 1rem; background: #ef4444; color: white; border: none; border-radius: 6px; cursor: pointer;">Logout</button>
                            </div>
                        </div>
                        <div class="chat-content">
                            <div id="messages" class="messages">
                                <div style="text-align: center; color: #6b7280; margin-top: 2rem;">
                                    <h2 style="margin-bottom: 0.5rem;">Welcome to Kultivate AI</h2>
                                    <p>Start asking questions about your data</p>
                                </div>
                            </div>
                            <div class="input-area">
                                <input id="messageInput" class="message-input" type="text" placeholder="Ask about your data..." onkeydown="if(event.key==='Enter') sendMessage()">
                                <button onclick="sendMessage()" class="send-btn">Send</button>
                            </div>
                        </div>
                    </div>
                </div>
            \`;
        }
        
        async function logout() {
            await fetch('/api/logout', { method: 'POST' });
            location.reload();
        }
        
        async function sendMessage() {
            const input = document.getElementById('messageInput');
            const messages = document.getElementById('messages');
            const message = input.value.trim();
            if (!message) return;
            
            // Add user message
            messages.innerHTML += \`<div class="user-message"><div class="user-bubble">\${message}</div></div>\`;
            input.value = '';
            
            // Add loading message
            messages.innerHTML += \`<div id="loading" class="ai-message"><div class="ai-bubble loading">AI is thinking...</div></div>\`;
            messages.scrollTop = messages.scrollHeight;
            
            try {
                const response = await fetch('/api/chat', {
                    method: 'POST',
                    headers: { 'Content-Type': 'application/json' },
                    body: JSON.stringify({ message, conversation_history: [] })
                });
                
                const data = await response.json();
                document.getElementById('loading').remove();
                
                if (data.error) {
                    messages.innerHTML += \`<div class="ai-message"><div class="ai-bubble error-bubble">Error: \${data.error}</div></div>\`;
                } else {
                    messages.innerHTML += \`<div class="ai-message"><div class="ai-bubble">\${data.reply || 'No response'}</div></div>\`;
                }
                messages.scrollTop = messages.scrollHeight;
            } catch (error) {
                document.getElementById('loading').remove();
                messages.innerHTML += \`<div class="ai-message"><div class="ai-bubble error-bubble">Error: \${error.message}</div></div>\`;
                messages.scrollTop = messages.scrollHeight;
            }
        }
        
        // Check if user is already logged in
        fetch('/api/user')
            .then(response => {
                if (response.ok) {
                    return response.json().then(user => {
                        showChatInterface(user);
                    });
                }
            })
            .catch(() => {
                // Show login form (already displayed)
            });
    </script>
</body>
</html>`);
    });
    
    console.log("Authentication and routes configured");
  } catch (error) {
    console.error("Error setting up server:", error);
  }
}

app.listen(PORT, "0.0.0.0", async () => {
  console.log(`Kultivate AI serving on port ${PORT}`);
  await initializeServer();
});