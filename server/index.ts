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
        flaskReady = true;
        flaskServerReady = true; // Set flag for proxy
        console.log('Flask server is ready');
        resolve();
      }
      
      // Also check for alternative Flask ready signals
      if ((output.includes('Starting Flask server') || output.includes('Serving Flask app')) && !flaskReady) {
        // Give Flask a few seconds to fully initialize
        setTimeout(() => {
          if (!flaskReady) {
            flaskReady = true;
            console.log('Flask server ready (alternative detection)');
            resolve();
          }
        }, 10000);
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

// Track Flask server readiness
let flaskServerReady = false;

// Handle conversation messages with database persistence BEFORE general proxy
app.post('/api/conversations/:id/messages', express.json(), async (req, res) => {
  if (!flaskServerReady) {
    return res.status(503).json({ error: 'Backend not ready' });
  }

  try {
    const conversationId = req.params.id;
    const { content } = req.body;

    console.log(`Processing message for conversation ${conversationId}: ${content}`);

    // Call Flask backend for AI processing
    const { default: fetch } = await import('node-fetch');
    const backendResponse = await fetch('http://localhost:8081/api/chat', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({
        message: content,
        conversation_history: []
      })
    });

    if (!backendResponse.ok) {
      throw new Error(`Backend responded with ${backendResponse.status}`);
    }

    const backendResult = await backendResponse.json();
    console.log('Backend response:', JSON.stringify(backendResult, null, 2));

    // Extract assistant message and displays from backend response
    const assistantContent = backendResult.reply || backendResult.final_answer || "No response";
    const displays = backendResult.displays || [];

    console.log(`Extracted displays: ${displays.length} items`);

    // Create response in the format expected by frontend
    const response = {
      userMessage: {
        id: `user-${Date.now()}`,
        role: 'user',
        content: content,
        timestamp: new Date().toISOString()
      },
      assistantMessage: {
        id: `assistant-${Date.now()}`,
        role: 'assistant', 
        content: assistantContent,
        displays: displays,
        timestamp: new Date().toISOString()
      }
    };

    console.log(`Sending response with ${displays.length} displays`);
    res.json(response);

  } catch (error) {
    console.error('Message processing error:', error);
    res.status(500).json({ error: 'Failed to process message' });
  }
});

// Proxy other /api requests to Flask backend
app.use('/api', (req, res, next) => {
  // If Flask not ready, wait briefly and retry
  if (!flaskServerReady) {
    setTimeout(() => {
      handleProxyRequest(req, res);
    }, 1000);
    return;
  }
  
  handleProxyRequest(req, res);
});

function handleProxyRequest(req: any, res: any) {
  const targetUrl = `http://localhost:8081${req.originalUrl}`;
  console.log(`Manual proxy: ${req.method} ${req.originalUrl} -> ${targetUrl}`);
  
  const options = {
    method: req.method,
    headers: {
      ...req.headers,
      host: 'localhost:8081'
    }
  };
  
  if (req.body && Object.keys(req.body).length > 0) {
    options.body = JSON.stringify(req.body);
    options.headers['content-type'] = 'application/json';
  }
  
  import('node-fetch').then(({ default: fetch }) => {
    fetch(targetUrl, options)
      .then(response => {
        res.status(response.status);
        return response.text();
      })
      .then(data => {
        try {
          const json = JSON.parse(data);
          res.json(json);
        } catch {
          res.send(data);
        }
      })
      .catch(error => {
        console.error('Proxy Error:', error.message);
        res.status(500).json({ error: 'Backend connection failed' });
      });
  });
}

// Health check
app.get('/health', (req, res) => {
  res.json({ status: 'ok', timestamp: new Date().toISOString() });
});

// Setup authentication and routes
async function initializeServer() {
  try {
    // Register non-API routes with authentication (if needed)
    // await registerRoutes(app);
    
    // Serve static files from client dist directory
    app.use(express.static(path.join(__dirname, '../dist')));
    
    // Serve React app for all other routes
    app.get('*', (req, res) => {
      res.sendFile(path.join(__dirname, '../dist/index.html'));
    });
    
    console.log("Authentication and routes configured");
    
    // Start Flask backend AFTER Node.js is ready
    await startFlaskServer();
    
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