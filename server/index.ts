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
    
    // Serve the React application for all routes
    app.get('*', (req, res) => {
      res.send(`<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="UTF-8" />
    <meta name="viewport" content="width=device-width, initial-scale=1.0" />
    <title>Kultivate AI</title>
    <style>
      * { margin: 0; padding: 0; box-sizing: border-box; }
      body { 
        font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        min-height: 100vh;
        display: flex;
        align-items: center;
        justify-content: center;
      }
      .auth-container {
        background: white;
        padding: 2rem;
        border-radius: 12px;
        box-shadow: 0 20px 40px rgba(0,0,0,0.1);
        width: 100%;
        max-width: 400px;
        text-align: center;
      }
      .logo {
        width: 60px;
        height: 60px;
        background: #eab308;
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
      .highlight { color: #eab308; }
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
        color: #eab308;
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
        border-color: #eab308;
      }
      .btn {
        width: 100%;
        padding: 0.875rem;
        background: #eab308;
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
        background: #ca8a04;
        transform: translateY(-1px);
      }
      .btn:disabled {
        opacity: 0.6;
        cursor: not-allowed;
        transform: none;
      }
      .error {
        color: #ef4444;
        font-size: 14px;
        margin-top: 0.5rem;
        text-align: center;
      }
      .success {
        color: #10b981;
        font-size: 14px;
        margin-top: 0.5rem;
        text-align: center;
      }
      .grid {
        display: grid;
        grid-template-columns: 1fr 1fr;
        gap: 1rem;
      }
    </style>
  </head>
  <body>
    <div id="root"></div>
    
    <script type="text/babel">
      const { useState } = React;
      
      function AuthApp() {
        const [activeTab, setActiveTab] = useState('login');
        const [loading, setLoading] = useState(false);
        const [message, setMessage] = useState('');
        const [loginData, setLoginData] = useState({ username: '', password: '' });
        const [registerData, setRegisterData] = useState({
          username: '', password: '', email: '', firstName: '', lastName: ''
        });
        
        const handleLogin = async (e) => {
          e.preventDefault();
          setLoading(true);
          setMessage('');
          
          try {
            const response = await fetch('/api/login', {
              method: 'POST',
              headers: { 'Content-Type': 'application/json' },
              body: JSON.stringify(loginData)
            });
            
            if (response.ok) {
              setMessage('Login successful! Redirecting...');
              setTimeout(() => window.location.reload(), 1000);
            } else {
              const error = await response.text();
              setMessage('Login failed: ' + error);
            }
          } catch (error) {
            setMessage('Login failed: ' + error.message);
          }
          setLoading(false);
        };
        
        const handleRegister = async (e) => {
          e.preventDefault();
          setLoading(true);
          setMessage('');
          
          try {
            const response = await fetch('/api/register', {
              method: 'POST',
              headers: { 'Content-Type': 'application/json' },
              body: JSON.stringify(registerData)
            });
            
            if (response.ok) {
              setMessage('Registration successful! Redirecting...');
              setTimeout(() => window.location.reload(), 1000);
            } else {
              const error = await response.text();
              setMessage('Registration failed: ' + error);
            }
          } catch (error) {
            setMessage('Registration failed: ' + error.message);
          }
          setLoading(false);
        };
        
        return (
          <div className="auth-container">
            <div className="logo">KA</div>
            <h1>Kultivate <span className="highlight">AI</span></h1>
            <p>Sign in to access your AI-powered data insights</p>
            
            <div className="tabs">
              <button 
                className={\`tab \${activeTab === 'login' ? 'active' : ''}\`}
                onClick={() => setActiveTab('login')}
              >
                Login
              </button>
              <button 
                className={\`tab \${activeTab === 'register' ? 'active' : ''}\`}
                onClick={() => setActiveTab('register')}
              >
                Register
              </button>
            </div>
            
            <form 
              className={\`form \${activeTab === 'login' ? 'active' : ''}\`}
              onSubmit={handleLogin}
            >
              <div className="input-group">
                <label>Username</label>
                <input 
                  type="text" 
                  value={loginData.username}
                  onChange={(e) => setLoginData({...loginData, username: e.target.value})}
                  required 
                />
              </div>
              <div className="input-group">
                <label>Password</label>
                <input 
                  type="password" 
                  value={loginData.password}
                  onChange={(e) => setLoginData({...loginData, password: e.target.value})}
                  required 
                />
              </div>
              <button type="submit" className="btn" disabled={loading}>
                {loading ? 'Signing in...' : 'Sign In'}
              </button>
            </form>
            
            <form 
              className={\`form \${activeTab === 'register' ? 'active' : ''}\`}
              onSubmit={handleRegister}
            >
              <div className="input-group">
                <label>Username</label>
                <input 
                  type="text" 
                  value={registerData.username}
                  onChange={(e) => setRegisterData({...registerData, username: e.target.value})}
                  required 
                />
              </div>
              <div className="input-group">
                <label>Password</label>
                <input 
                  type="password" 
                  value={registerData.password}
                  onChange={(e) => setRegisterData({...registerData, password: e.target.value})}
                  required 
                />
              </div>
              <div className="input-group">
                <label>Email (optional)</label>
                <input 
                  type="email" 
                  value={registerData.email}
                  onChange={(e) => setRegisterData({...registerData, email: e.target.value})}
                />
              </div>
              <div className="grid">
                <div className="input-group">
                  <label>First Name</label>
                  <input 
                    type="text" 
                    value={registerData.firstName}
                    onChange={(e) => setRegisterData({...registerData, firstName: e.target.value})}
                  />
                </div>
                <div className="input-group">
                  <label>Last Name</label>
                  <input 
                    type="text" 
                    value={registerData.lastName}
                    onChange={(e) => setRegisterData({...registerData, lastName: e.target.value})}
                  />
                </div>
              </div>
              <button type="submit" className="btn" disabled={loading}>
                {loading ? 'Creating account...' : 'Create Account'}
              </button>
            </form>
            
            {message && (
              <div className={message.includes('successful') ? 'success' : 'error'}>
                {message}
              </div>
            )}
          </div>
        );
      }
      
      // Check if user is already logged in
      fetch('/api/user')
        .then(response => {
          if (response.ok) {
            // User is logged in, show chat interface
            return response.json().then(user => {
              document.body.innerHTML = \`
                <div style="display: flex; height: 100vh; background: #f9fafb;">
                  <div style="flex: 1; display: flex; flex-direction: column;">
                    <header style="background: white; border-bottom: 1px solid #e5e7eb; padding: 1rem; display: flex; justify-content: space-between; align-items: center;">
                      <div style="display: flex; align-items: center;">
                        <div style="width: 32px; height: 32px; background: #eab308; border-radius: 6px; display: flex; align-items: center; justify-content: center; color: white; font-weight: bold; margin-right: 0.5rem;">KA</div>
                        <h1 style="font-size: 1.25rem; font-weight: 600;">Kultivate <span style="color: #eab308;">AI</span></h1>
                      </div>
                      <div style="display: flex; align-items: center; gap: 1rem;">
                        <span style="color: #6b7280;">Welcome, \${user.username}</span>
                        <button onclick="logout()" style="padding: 0.5rem 1rem; background: #ef4444; color: white; border: none; border-radius: 6px; cursor: pointer;">Logout</button>
                      </div>
                    </header>
                    <div style="flex: 1; display: flex; flex-direction: column; padding: 1rem;">
                      <div id="messages" style="flex: 1; background: white; border-radius: 8px; padding: 1rem; margin-bottom: 1rem; overflow-y: auto;">
                        <div style="text-align: center; color: #6b7280; margin-top: 2rem;">
                          <h2 style="margin-bottom: 0.5rem;">Welcome to Kultivate AI</h2>
                          <p>Start asking questions about your data</p>
                        </div>
                      </div>
                      <div style="display: flex; gap: 0.5rem;">
                        <input id="messageInput" type="text" placeholder="Ask about your data..." style="flex: 1; padding: 0.75rem; border: 1px solid #d1d5db; border-radius: 6px; font-size: 16px;" onkeydown="if(event.key==='Enter') sendMessage()">
                        <button onclick="sendMessage()" style="padding: 0.75rem 1rem; background: #eab308; color: white; border: none; border-radius: 6px; cursor: pointer; font-weight: 500;">Send</button>
                      </div>
                    </div>
                  </div>
                </div>
                <script>
                  window.logout = async function() {
                    await fetch('/api/logout', { method: 'POST' });
                    window.location.reload();
                  }
                  
                  window.sendMessage = async function() {
                    const input = document.getElementById('messageInput');
                    const messages = document.getElementById('messages');
                    const message = input.value.trim();
                    if (!message) return;
                    
                    // Add user message
                    messages.innerHTML += \`<div style="margin-bottom: 1rem; text-align: right;"><div style="display: inline-block; background: #eab308; color: white; padding: 0.75rem; border-radius: 8px; max-width: 70%;">\${message}</div></div>\`;
                    input.value = '';
                    
                    // Add loading message
                    messages.innerHTML += \`<div id="loading" style="margin-bottom: 1rem;"><div style="display: inline-block; background: #f3f4f6; padding: 0.75rem; border-radius: 8px; color: #6b7280;">AI is thinking...</div></div>\`;
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
                        messages.innerHTML += \`<div style="margin-bottom: 1rem;"><div style="display: inline-block; background: #fef2f2; color: #dc2626; padding: 0.75rem; border-radius: 8px; max-width: 70%;">Error: \${data.error}</div></div>\`;
                      } else {
                        messages.innerHTML += \`<div style="margin-bottom: 1rem;"><div style="display: inline-block; background: #f3f4f6; padding: 0.75rem; border-radius: 8px; max-width: 70%;">\${data.reply || 'No response'}</div></div>\`;
                      }
                      messages.scrollTop = messages.scrollHeight;
                    } catch (error) {
                      document.getElementById('loading').remove();
                      messages.innerHTML += \`<div style="margin-bottom: 1rem;"><div style="display: inline-block; background: #fef2f2; color: #dc2626; padding: 0.75rem; border-radius: 8px; max-width: 70%;">Error: \${error.message}</div></div>\`;
                      messages.scrollTop = messages.scrollHeight;
                    }
                  }
                </script>
              \`;
            });
          } else {
            // User not logged in, show auth form
            const root = ReactDOM.createRoot(document.getElementById('root'));
            root.render(React.createElement(AuthApp));
          }
        })
        .catch(() => {
          // Error checking auth, show login form
          const root = ReactDOM.createRoot(document.getElementById('root'));
          root.render(React.createElement(AuthApp));
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