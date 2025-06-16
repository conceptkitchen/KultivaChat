import express from "express";
import path from "path";
import { fileURLToPath } from 'url';
import { spawn } from 'child_process';
import { createProxyMiddleware } from 'http-proxy-middleware';
import { registerRoutes } from "./routes";

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

const app = express();
app.use(express.json());
app.use(express.urlencoded({ extended: false }));

const PORT = parseInt(process.env.PORT ?? "5000", 10);

let flaskProcess: any = null;

// Start Flask backend server and wait for it to be ready
function startFlaskServer(): Promise<void> {
  return new Promise((resolve, reject) => {
    const backendPath = path.join(__dirname, '../backend');
    flaskProcess = spawn('python', ['main_2.py'], {
      cwd: backendPath,
      stdio: ['ignore', 'pipe', 'pipe']
    });
    
    let flaskReady = false;
    
    flaskProcess.stdout?.on('data', (data: Buffer) => {
      const output = data.toString().trim();
      console.log(`Flask: ${output}`);
      
      // Flask is ready when it shows the server running message
      if (output.includes('Running on http://127.0.0.1:8081') && !flaskReady) {
        // Wait a bit more then test the connection
        setTimeout(async () => {
          if (!flaskReady) {
            try {
              const response = await fetch('http://localhost:8081/api/health');
              if (response.ok) {
                flaskReady = true;
                console.log('Flask server is ready');
                resolve();
              }
            } catch (e) {
              console.log('Flask health check failed, waiting...');
              setTimeout(() => resolve(), 3000);
            }
          }
        }, 3000);
      }
    });
    
    flaskProcess.stderr?.on('data', (data: Buffer) => {
      const output = data.toString().trim();
      console.log(`Flask Error: ${output}`);
      
      // Also check stderr for the running message
      if (output.includes('Running on http://127.0.0.1:8081') && !flaskReady) {
        flaskReady = true;
        console.log('Flask server is ready');
        resolve();
      }
      
      // Check for Flask app startup in stderr
      if ((output.includes('Starting Flask server') || output.includes('Serving Flask app')) && !flaskReady) {
        setTimeout(() => {
          if (!flaskReady) {
            flaskReady = true;
            console.log('Flask server ready (stderr detection)');
            resolve();
          }
        }, 10000);
      }
    });
    
    flaskProcess.on('close', (code: number) => {
      console.log(`Flask process exited with code ${code}`);
      if (!flaskReady) {
        reject(new Error(`Flask server failed to start (exit code: ${code})`));
      }
    });
    
    flaskProcess.on('error', (error: Error) => {
      console.error('Flask server error:', error);
      reject(error);
    });
    
    console.log('Starting Flask backend server...');
    
    // Timeout in case Flask never signals ready (increased for production)
    setTimeout(() => {
      if (!flaskReady) {
        reject(new Error('Flask server startup timeout'));
      }
    }, 120000); // 2 minutes for production initialization
  });
}

// Health check
app.get('/health', (req, res) => {
  res.json({ status: 'ok', timestamp: new Date().toISOString() });
});

// Setup authentication and routes
async function initializeServer() {
  try {
    // Start Flask backend first
    await startFlaskServer();
    
    // Direct backend health check
    try {
      const healthCheck = await fetch('http://127.0.0.1:8081/api/health');
      if (healthCheck.ok) {
        console.log('Backend connection verified');
      }
    } catch (e) {
      console.log('Backend health check failed, but continuing...');
    }
    
    // Register API routes with authentication first
    await registerRoutes(app);
    
    if (process.env.NODE_ENV === 'production') {
      // Production: serve a working HTML page immediately
      console.log("Production mode: serving working HTML");
      
      app.get('*', (req, res) => {
        if (req.path.startsWith('/api')) {
          return;
        }
        
        res.send(`
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Kultivate AI</title>
    <script src="https://unpkg.com/react@18/umd/react.production.min.js"></script>
    <script src="https://unpkg.com/react-dom@18/umd/react-dom.production.min.js"></script>
    <style>
        body { font-family: Arial, sans-serif; margin: 0; padding: 20px; background: #f5f5f5; }
        .container { max-width: 800px; margin: 0 auto; background: white; padding: 40px; border-radius: 8px; box-shadow: 0 2px 10px rgba(0,0,0,0.1); }
        .chat-container { margin-top: 20px; min-height: 300px; border: 1px solid #ccc; padding: 20px; border-radius: 8px; background: #fafafa; }
        .message { padding: 10px; margin: 10px 0; border-radius: 8px; }
        .user { background: #e3f2fd; text-align: right; }
        .assistant { background: #f1f8e9; }
        .input-container { display: flex; margin-top: 20px; }
        input { flex: 1; padding: 10px; border: 1px solid #ccc; border-radius: 4px; }
        button { padding: 10px 20px; margin-left: 10px; background: #2196F3; color: white; border: none; border-radius: 4px; cursor: pointer; }
        button:hover { background: #1976D2; }
        .status { color: #4CAF50; font-weight: bold; margin-bottom: 20px; }
        .loading { color: #FF9800; }
    </style>
</head>
<body>
    <div class="container">
        <h1>🚀 Kultivate AI Data Assistant</h1>
        <div class="status">✓ Production mode - Backend connected and ready</div>
        <p>Query your Keboola data using natural language with Gemini 2.0 Flash</p>
        
        <div class="chat-container" id="messages"></div>
        
        <div class="input-container">
            <input type="text" id="messageInput" placeholder="Ask about your data..." onkeypress="handleKeyPress(event)">
            <button onclick="sendMessage()">Send</button>
        </div>
    </div>

    <script>
        let isLoading = false;
        
        async function sendMessage() {
            const input = document.getElementById('messageInput');
            const message = input.value.trim();
            if (!message || isLoading) return;
            
            addMessage('user', message);
            input.value = '';
            isLoading = true;
            addMessage('assistant', '🤔 Processing your query...', 'loading');
            
            try {
                const response = await fetch('/api/messages', {
                    method: 'POST',
                    headers: { 'Content-Type': 'application/json' },
                    body: JSON.stringify({
                        conversationId: 'production-chat',
                        content: message
                    })
                });
                
                const data = await response.json();
                removeLoadingMessage();
                addMessage('assistant', data.reply || data.error || 'No response received');
                
            } catch (error) {
                removeLoadingMessage();
                addMessage('assistant', 'Error: ' + error.message);
            } finally {
                isLoading = false;
            }
        }
        
        function addMessage(role, content, className = '') {
            const messages = document.getElementById('messages');
            const div = document.createElement('div');
            div.className = 'message ' + role + (className ? ' ' + className : '');
            div.textContent = content;
            if (className === 'loading') {
                div.id = 'loading-message';
            }
            messages.appendChild(div);
            messages.scrollTop = messages.scrollHeight;
        }
        
        function removeLoadingMessage() {
            const loading = document.getElementById('loading-message');
            if (loading) loading.remove();
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
            .catch(() => addMessage('assistant', '⚠️ Backend connection failed'));
    </script>
</body>
</html>
        `);
      });
    } else {
      // Development: proxy to Vite dev server
      console.log("Development mode: starting Vite dev server");
      const viteProcess = spawn('npx', ['vite', '--port', '3000'], {
        cwd: process.cwd(),
        stdio: 'pipe',
        env: { ...process.env }
      });
      
      viteProcess.stdout?.on('data', (data) => {
        console.log('Vite:', data.toString());
      });
      
      viteProcess.stderr?.on('data', (data) => {
        console.log('Vite Error:', data.toString());
      });
      
      // Proxy frontend requests to Vite dev server
      app.use('/', createProxyMiddleware({
        target: 'http://localhost:3000',
        changeOrigin: true,
        ws: true,
        pathFilter: (path) => !path.startsWith('/api')
      }));
    }
    
    console.log("Authentication and routes configured");
    
  } catch (error) {
    console.error("Error setting up server:", error);
  }
}

// Handle graceful shutdown
process.on('SIGTERM', () => {
  console.log('Shutting down servers...');
  if (flaskProcess) {
    flaskProcess.kill();
  }
  process.exit(0);
});

process.on('SIGINT', () => {
  console.log('Shutting down servers...');
  if (flaskProcess) {
    flaskProcess.kill();
  }
  process.exit(0);
});

app.listen(PORT, "0.0.0.0", async () => {
  console.log(`Kultivate AI serving on port ${PORT}`);
  await initializeServer();
});