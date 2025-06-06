import express from "express";
import path from "path";
import { fileURLToPath } from 'url';
import { setupAuth } from "./replitAuth";
import { registerRoutes } from "./routes";
import { registerUnauthedRoutes } from "./routes-unauthed";

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

const app = express();
app.use(express.json());
app.use(express.urlencoded({ extended: false }));

// Health check
app.get('/health', (req, res) => {
  res.json({ status: 'ok', timestamp: new Date().toISOString() });
});

// Serve the login page or redirect to app if authenticated
app.get('/', (req: any, res) => {
  // If user is already authenticated, redirect to app
  if (req.user) {
    return res.redirect('/app');
  }
  res.send(`<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Kultivate AI Assistant</title>
    <style>
        body {
            margin: 0;
            font-family: 'Inter', -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
            background: #f9fafb;
            display: flex;
            align-items: center;
            justify-content: center;
            min-height: 100vh;
            padding: 20px;
        }
        .login-card {
            background: white;
            padding: 2rem;
            border-radius: 8px;
            box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
            text-align: center;
            max-width: 400px;
            width: 100%;
        }
        .logo {
            width: 48px;
            height: 48px;
            background: #eab308;
            border-radius: 6px;
            display: flex;
            align-items: center;
            justify-content: center;
            margin: 0 auto 1rem;
        }
        .title {
            font-size: 1.5rem;
            font-weight: 700;
            margin: 0 0 0.5rem;
            color: #1f2937;
        }
        .subtitle {
            color: #6b7280;
            margin: 0 0 2rem;
            line-height: 1.5;
        }
        .login-btn {
            background: #eab308;
            color: white;
            padding: 10px 20px;
            border: none;
            border-radius: 6px;
            font-weight: 500;
            text-decoration: none;
            display: inline-block;
            cursor: pointer;
            transition: background-color 0.15s;
        }
        .login-btn:hover {
            background: #ca8a04;
        }
        .highlight {
            color: #eab308;
        }
    </style>
</head>
<body>
    <div class="login-card">
        <div class="logo">
            <svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="white" stroke-width="2.5" stroke-linecap="round" stroke-linejoin="round">
                <path d="M12 8V4H8"></path>
                <rect width="16" height="12" x="4" y="8" rx="2"></rect>
                <path d="M2 14h2"></path>
                <path d="M20 14h2"></path>
                <path d="M15 13v2"></path>
                <path d="M9 13v2"></path>
            </svg>
        </div>
        <h1 class="title">
            Kultivate <span class="highlight">AI</span>
        </h1>
        <p class="subtitle">
            Your AI-powered data integration platform for intelligent exploration and visualization
        </p>
        <a href="/api/login" class="login-btn">
            Log In to Continue
        </a>
    </div>
</body>
</html>`);
});

// Serve the main React application for authenticated users
app.get('/app', (req, res) => {
  res.send(`<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="UTF-8" />
    <meta name="viewport" content="width=device-width, initial-scale=1.0" />
    <title>Kultivate AI Assistant</title>
    <script type="importmap">
    {
      "imports": {
        "react": "https://esm.sh/react@18.2.0",
        "react-dom/client": "https://esm.sh/react-dom@18.2.0/client",
        "wouter": "https://esm.sh/wouter@3.0.0",
        "@tanstack/react-query": "https://esm.sh/@tanstack/react-query@5.0.0"
      }
    }
    </script>
    <style>
      * { box-sizing: border-box; margin: 0; padding: 0; }
      body { 
        font-family: 'Inter', sans-serif; 
        background: #f5f5f5; 
        height: 100vh; 
        overflow: hidden;
      }
      .app-container {
        display: flex;
        height: 100vh;
        background: #f9fafb;
      }
      .header {
        background: white;
        border-bottom: 1px solid #e5e7eb;
        padding: 1rem;
        display: flex;
        align-items: center;
        justify-content: between;
      }
      .logo {
        width: 32px;
        height: 32px;
        background: #eab308;
        border-radius: 4px;
        display: flex;
        align-items: center;
        justify-content: center;
        margin-right: 0.5rem;
      }
      .chat-area {
        flex: 1;
        display: flex;
        flex-direction: column;
        background: white;
        margin: 1rem;
        border-radius: 8px;
        overflow: hidden;
      }
      .chat-input {
        padding: 1rem;
        border-top: 1px solid #e5e7eb;
      }
      .input-field {
        width: 100%;
        padding: 0.75rem;
        border: 1px solid #d1d5db;
        border-radius: 6px;
        font-size: 14px;
      }
      .send-button {
        background: #eab308;
        color: white;
        border: none;
        padding: 0.75rem 1rem;
        border-radius: 6px;
        margin-left: 0.5rem;
        cursor: pointer;
      }
      .welcome-message {
        flex: 1;
        display: flex;
        align-items: center;
        justify-content: center;
        flex-direction: column;
        color: #6b7280;
      }
    </style>
  </head>
  <body>
    <div id="root"></div>
    <script type="module">
      import React from 'react';
      import { createRoot } from 'react-dom/client';
      
      function App() {
        const [message, setMessage] = React.useState('');
        
        return React.createElement('div', { className: 'app-container' },
          React.createElement('div', { style: { flex: 1, display: 'flex', flexDirection: 'column' } },
            React.createElement('div', { className: 'header' },
              React.createElement('div', { style: { display: 'flex', alignItems: 'center' } },
                React.createElement('div', { className: 'logo' },
                  React.createElement('svg', {
                    width: '20', height: '20', viewBox: '0 0 24 24', fill: 'none',
                    stroke: 'white', strokeWidth: '2', strokeLinecap: 'round', strokeLinejoin: 'round'
                  },
                    React.createElement('path', { d: 'M12 8V4H8' }),
                    React.createElement('rect', { width: '16', height: '12', x: '4', y: '8', rx: '2' })
                  )
                ),
                React.createElement('h1', { style: { fontSize: '1.25rem', fontWeight: '600' } },
                  'Kultivate ', React.createElement('span', { style: { color: '#eab308' } }, 'AI')
                )
              )
            ),
            React.createElement('div', { className: 'chat-area' },
              React.createElement('div', { className: 'welcome-message' },
                React.createElement('h2', { style: { marginBottom: '0.5rem' } }, 'Welcome to Kultivate AI'),
                React.createElement('p', null, 'Start a conversation with your data assistant')
              ),
              React.createElement('div', { className: 'chat-input' },
                React.createElement('div', { style: { display: 'flex' } },
                  React.createElement('input', {
                    className: 'input-field',
                    placeholder: 'Ask me about your data...',
                    value: message,
                    onChange: (e) => setMessage(e.target.value)
                  }),
                  React.createElement('button', {
                    className: 'send-button',
                    onClick: () => {
                      if (message.trim()) {
                        alert('Chat functionality will be connected to backend');
                        setMessage('');
                      }
                    }
                  }, 'Send')
                )
              )
            )
          )
        );
      }
      
      const root = createRoot(document.getElementById('root'));
      root.render(React.createElement(App));
    </script>
  </body>
</html>`);
});

const PORT = Number(process.env.PORT) || 5000;

// Setup authentication and routes
async function initializeServer() {
  try {
    // Setup authentication system
    await setupAuth(app);
    
    // Register API routes
    await registerRoutes(app);
    await registerUnauthedRoutes(app);
    
    console.log("Authentication and routes configured");
  } catch (error) {
    console.error("Error setting up auth:", error);
  }
}

app.listen(PORT, "0.0.0.0", async () => {
  console.log(`Kultivate AI serving on port ${PORT}`);
  await initializeServer();
});