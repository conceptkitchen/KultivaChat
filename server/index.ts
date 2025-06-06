import express from "express";
import path from "path";
import { fileURLToPath } from 'url';
import { setupAuth } from "./replitAuth";
import { registerRoutes } from "./routes";
import { registerUnauthedRoutes } from "./routes-unauthed";
import fetch from 'node-fetch';

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
        const [messages, setMessages] = React.useState([]);
        const [isLoading, setIsLoading] = React.useState(false);
        
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
              messages.length === 0 
                ? React.createElement('div', { className: 'welcome-message' },
                    React.createElement('h2', { style: { marginBottom: '0.5rem' } }, 'Welcome to Kultivate AI'),
                    React.createElement('p', null, 'Start a conversation with your data assistant')
                  )
                : React.createElement('div', { 
                    style: { 
                      flex: 1, 
                      padding: '1rem', 
                      overflowY: 'auto',
                      display: 'flex',
                      flexDirection: 'column',
                      gap: '1rem'
                    } 
                  },
                    ...messages.map((msg, idx) => 
                      React.createElement('div', { 
                        key: idx,
                        style: { 
                          display: 'flex', 
                          justifyContent: msg.role === 'user' ? 'flex-end' : 'flex-start',
                          flexDirection: 'column',
                          alignItems: msg.role === 'user' ? 'flex-end' : 'flex-start'
                        } 
                      },
                        React.createElement('div', {
                          style: {
                            maxWidth: '70%',
                            padding: '0.75rem',
                            borderRadius: '8px',
                            backgroundColor: msg.role === 'user' ? '#eab308' : '#f3f4f6',
                            color: msg.role === 'user' ? 'white' : '#374151',
                            whiteSpace: 'pre-wrap'
                          }
                        }, msg.content),
                        
                        // Render data displays if available
                        msg.displays && msg.displays.length > 0 && React.createElement('div', {
                          style: {
                            maxWidth: '90%',
                            marginTop: '0.5rem',
                            padding: '1rem',
                            backgroundColor: 'white',
                            border: '1px solid #e5e7eb',
                            borderRadius: '8px'
                          }
                        },
                          ...msg.displays.map((display, displayIdx) => 
                            React.createElement('div', { key: displayIdx },
                              React.createElement('h4', { 
                                style: { marginBottom: '0.5rem', color: '#374151', fontSize: '1rem', fontWeight: '600' } 
                              }, display.title || 'Data Table'),
                              
                              display.type === 'table' && display.content && React.createElement('div', {
                                style: { 
                                  maxHeight: '300px', 
                                  overflowY: 'auto',
                                  border: '1px solid #d1d5db',
                                  borderRadius: '4px'
                                }
                              },
                                React.createElement('table', {
                                  style: { 
                                    width: '100%', 
                                    borderCollapse: 'collapse',
                                    fontSize: '0.875rem'
                                  }
                                },
                                  React.createElement('thead', {},
                                    React.createElement('tr', { 
                                      style: { backgroundColor: '#f9fafb' } 
                                    },
                                      React.createElement('th', {
                                        style: { 
                                          padding: '0.5rem', 
                                          textAlign: 'left',
                                          borderBottom: '1px solid #d1d5db',
                                          fontWeight: '600'
                                        }
                                      }, 'Table Name')
                                    )
                                  ),
                                  React.createElement('tbody', {},
                                    ...display.content.slice(0, 20).map((row, rowIdx) =>
                                      React.createElement('tr', { 
                                        key: rowIdx,
                                        style: { 
                                          borderBottom: '1px solid #e5e7eb',
                                          '&:hover': { backgroundColor: '#f9fafb' }
                                        }
                                      },
                                        React.createElement('td', {
                                          style: { 
                                            padding: '0.5rem',
                                            fontFamily: 'monospace',
                                            fontSize: '0.8rem'
                                          }
                                        }, row.table_name || JSON.stringify(row))
                                      )
                                    )
                                  )
                                )
                              ),
                              
                              display.content && display.content.length > 20 && React.createElement('p', {
                                style: { 
                                  marginTop: '0.5rem', 
                                  fontSize: '0.875rem', 
                                  color: '#6b7280',
                                  fontStyle: 'italic'
                                }
                              }, 'Showing first 20 of ' + display.content.length + ' tables')
                            )
                          )
                        )
                      )
                    ),
                    isLoading && React.createElement('div', { 
                      style: { 
                        display: 'flex', 
                        justifyContent: 'flex-start' 
                      } 
                    },
                      React.createElement('div', {
                        style: {
                          padding: '0.75rem',
                          borderRadius: '8px',
                          backgroundColor: '#f3f4f6',
                          color: '#6b7280'
                        }
                      }, 'AI is thinking...')
                    )
                  ),
              React.createElement('div', { className: 'chat-input' },
                React.createElement('div', { style: { display: 'flex' } },
                  React.createElement('input', {
                    className: 'input-field',
                    placeholder: 'Ask me about your data...',
                    value: message,
                    onChange: (e) => setMessage(e.target.value),
                    onKeyDown: (e) => {
                      if (e.key === 'Enter' && message.trim() && !isLoading) {
                        e.preventDefault();
                        const userMessage = message.trim();
                        setMessages(prev => [...prev, { role: 'user', content: userMessage }]);
                        setMessage('');
                        setIsLoading(true);
                        
                        fetch('/api/chat', {
                          method: 'POST',
                          headers: { 'Content-Type': 'application/json' },
                          body: JSON.stringify({ message: userMessage })
                        })
                        .then(response => response.json())
                        .then(data => {
                          if (data.error) throw new Error(data.error);
                          let content = data.reply || data.response || data.message || 'AI response received';
                          setMessages(prev => [...prev, { 
                            role: 'assistant', 
                            content: content,
                            displays: data.displays || []
                          }]);
                        })
                        .catch(error => {
                          setMessages(prev => [...prev, { 
                            role: 'assistant', 
                            content: 'Sorry, I had trouble connecting to the data backend. Please try again.'
                          }]);
                        })
                        .finally(() => setIsLoading(false));
                      }
                    }
                  }),
                  React.createElement('button', {
                    className: 'send-button',
                    disabled: isLoading,
                    onClick: async () => {
                      if (message.trim() && !isLoading) {
                        const userMessage = message.trim();
                        setMessages(prev => [...prev, { role: 'user', content: userMessage }]);
                        setMessage('');
                        setIsLoading(true);
                        
                        try {
                          const response = await fetch('/api/chat', {
                            method: 'POST',
                            headers: { 'Content-Type': 'application/json' },
                            body: JSON.stringify({ message: userMessage })
                          });
                          const data = await response.json();
                          
                          if (data.error) {
                            throw new Error(data.error);
                          }
                          
                          // Handle both simple responses and structured data
                          let content = data.reply || data.response || data.message || 'AI response received';
                          
                          setMessages(prev => [...prev, { 
                            role: 'assistant', 
                            content: content,
                            displays: data.displays || []
                          }]);
                        } catch (error) {
                          setMessages(prev => [...prev, { 
                            role: 'assistant', 
                            content: 'Sorry, I had trouble connecting to the data backend. Please try again.'
                          }]);
                          console.error('Chat error:', error);
                        } finally {
                          setIsLoading(false);
                        }
                      }
                    }
                  }, isLoading ? 'Sending...' : 'Send')
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

// Chat API endpoint that connects to Python backend
app.post('/api/chat', async (req: any, res) => {
  try {
    const { message } = req.body;
    
    if (!message) {
      return res.status(400).json({ error: 'Message is required' });
    }

    // Forward request to Python Flask backend
    const response = await fetch('http://127.0.0.1:8081/api/chat', {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
      },
      body: JSON.stringify({ message })
    });

    if (!response.ok) {
      throw new Error(`Backend responded with status: ${response.status}`);
    }

    const data = await response.json();
    res.json(data);
    
  } catch (error) {
    console.error('Chat API error:', error);
    res.status(500).json({ 
      error: 'Failed to connect to AI backend',
      details: error instanceof Error ? error.message : 'Unknown error'
    });
  }
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