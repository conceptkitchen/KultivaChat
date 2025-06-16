import express from "express";
import path from "path";
import { fileURLToPath } from 'url';
import { spawn } from 'child_process';
import { createProxyMiddleware } from 'http-proxy-middleware';
import { registerRoutes } from "./routes";
import { setupVite } from "./vite";

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
      // Production: serve built React app directly
      console.log("Production mode: serving built React app from dist");
      
      const distPath = path.join(__dirname, '../dist');
      const indexPath = path.join(distPath, 'index.html');
      
      // Serve static files
      app.use(express.static(distPath, {
        setHeaders: (res, filePath) => {
          if (filePath.endsWith('.html')) {
            res.setHeader('Content-Type', 'text/html');
          }
        }
      }));
      
      // Catch-all handler for SPA routing
      app.get("*", (req, res) => {
        // Don't serve React app for API routes
        if (req.path.startsWith('/api')) {
          return res.status(404).json({ error: 'API endpoint not found' });
        }
        
        // Check if index.html exists, if not create a simple one
        if (!require('fs').existsSync(indexPath)) {
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
                    <div class="mb-4">
                        <input type="text" id="chatInput" placeholder="Ask about your data..." 
                               class="w-full p-3 border border-gray-300 rounded-lg focus:outline-none focus:ring-2 focus:ring-blue-500"/>
                    </div>
                    <button onclick="sendMessage()" 
                            class="w-full bg-blue-600 text-white p-3 rounded-lg hover:bg-blue-700 transition-colors">
                        Send Message
                    </button>
                    <div id="chatResponse" class="mt-4 p-3 bg-gray-100 rounded-lg hidden">
                        <p class="text-gray-700">Response will appear here...</p>
                    </div>
                </div>
            </div>
        </div>
    </div>
    <script>
        async function sendMessage() {
            const input = document.getElementById('chatInput');
            const response = document.getElementById('chatResponse');
            const message = input.value.trim();
            
            if (!message) return;
            
            response.style.display = 'block';
            response.innerHTML = '<p class="text-gray-600">Processing your request...</p>';
            
            try {
                const result = await fetch('/api/chat', {
                    method: 'POST',
                    headers: { 'Content-Type': 'application/json' },
                    body: JSON.stringify({ message: message })
                });
                
                const data = await result.json();
                response.innerHTML = '<p class="text-gray-800">' + (data.response || 'No response received') + '</p>';
            } catch (error) {
                response.innerHTML = '<p class="text-red-600">Error: Unable to connect to backend</p>';
            }
            
            input.value = '';
        }
        
        document.getElementById('chatInput').addEventListener('keypress', function(e) {
            if (e.key === 'Enter') sendMessage();
        });
    </script>
</body>
</html>`;
          require('fs').writeFileSync(indexPath, simpleHTML);
        }
        
        res.setHeader('Content-Type', 'text/html');
        res.sendFile(indexPath);
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