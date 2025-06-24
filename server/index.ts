import express from "express";
import path from "path";
import { fileURLToPath } from 'url';
import { createProxyMiddleware } from 'http-proxy-middleware';
import { spawn, ChildProcess } from 'child_process';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

const app = express();
app.use(express.json());
app.use(express.urlencoded({ extended: false }));

// Load ports from environment variables with defaults
const PORT = parseInt(process.env.PORT ?? "5000", 10);
const BACKEND_URL = process.env.BACKEND_URL ?? 'http://localhost:8081';

console.log('Kultivate AI Frontend Server listening on port', PORT);
console.log('Proxying API requests to:', BACKEND_URL);

// Python Backend Service
class PythonBackendService {
  private backendProcess: ChildProcess | null = null;
  private isStarting = false;
  private isReady = false;

  async start(): Promise<void> {
    if (this.isStarting || this.backendProcess) {
      return;
    }

    this.isStarting = true;
    console.log('Starting Python backend server...');

    try {
      // Change to backend directory and start gunicorn
      this.backendProcess = spawn('gunicorn', [
        '--workers', '1',
        '--bind', '0.0.0.0:8081',
        'main_2:app'
      ], {
        cwd: path.join(__dirname, '../backend'),
        stdio: ['ignore', 'pipe', 'pipe'],
        detached: false // Keep as child process
      });

      // Handle process output
      this.backendProcess.stdout?.on('data', (data) => {
        const output = data.toString();
        console.log('[Backend]', output.trim());
        
        // Check if backend is ready - look for config completion
        if (output.includes('Listening at:') || output.includes('Gemini GenerateContentConfig with tools created successfully')) {
          this.isReady = true;
        }
      });

      this.backendProcess.stderr?.on('data', (data) => {
        const output = data.toString();
        console.error('[Backend Error]', output.trim());
        
        // Also check stderr for readiness indicators
        if (output.includes('Listening at:') || output.includes('Gemini GenerateContentConfig with tools created successfully')) {
          this.isReady = true;
        }
      });

      this.backendProcess.on('exit', (code, signal) => {
        console.log(`Backend process exited with code ${code}, signal ${signal}`);
        this.backendProcess = null;
        this.isReady = false;
        this.isStarting = false;
      });

      this.backendProcess.on('error', (error) => {
        console.error('Backend process error:', error);
        this.backendProcess = null;
        this.isReady = false;
        this.isStarting = false;
      });

      // Wait for backend to be ready
      await this.waitForReady();
      console.log('Python backend server is ready');

    } catch (error) {
      console.error('Failed to start backend:', error);
      this.isStarting = false;
      throw error;
    }
  }

  private async waitForReady(): Promise<void> {
    const maxWait = 60000; // 60 seconds - increased for complete backend initialization
    const start = Date.now();
    
    while (!this.isReady && (Date.now() - start) < maxWait) {
      await new Promise(resolve => setTimeout(resolve, 1000));
    }
    
    if (!this.isReady) {
      throw new Error('Backend failed to start within timeout');
    }
  }

  async stop(): Promise<void> {
    if (this.backendProcess) {
      console.log('Stopping Python backend server...');
      this.backendProcess.kill('SIGTERM');
      this.backendProcess = null;
      this.isReady = false;
    }
  }

  isBackendReady(): boolean {
    return this.isReady && this.backendProcess !== null;
  }
}

const backendService = new PythonBackendService();

// Middleware to ensure backend is ready
app.use('/api', async (req, res, next) => {
  if (!backendService.isBackendReady()) {
    console.log('Backend not ready, attempting to start...');
    try {
      await backendService.start();
    } catch (error) {
      console.error('Failed to start backend:', error);
      return res.status(503).json({ error: 'Backend service unavailable' });
    }
  }
  next();
});

// --- Unified API Proxy ---
// All requests to /api/* will be forwarded to the backend
app.use('/api', createProxyMiddleware({
  target: BACKEND_URL,
  changeOrigin: true,
  logLevel: 'silent',
  // Ensure request body is forwarded correctly
  onProxyReq: (proxyReq, req, res) => {
    if (req.body && Object.keys(req.body).length) {
      const bodyData = JSON.stringify(req.body);
      proxyReq.setHeader('Content-Type','application/json');
      proxyReq.setHeader('Content-Length', Buffer.byteLength(bodyData));
      proxyReq.write(bodyData);
    }
  },
  onError: (err, req, res) => {
    console.error('Proxy error for', req.url, ':', err.message);
    if (res && typeof res.writeHead === 'function') {
      res.writeHead(504, {'Content-Type': 'text/plain'});
      res.end('Gateway Timeout');
    }
  }
}));

// --- Frontend Server Logic ---

// Health check
app.get('/health', (req, res) => {
  res.json({ status: 'ok', timestamp: new Date().toISOString() });
});

async function initializeServer() {
  try {
    // Serve static files from the client's build directory
    app.use(express.static(path.join(__dirname, '../dist')));
    
    // For any other route, serve the React app's index.html file.
    // This is necessary for client-side routing to work correctly.
    app.get('*', (req, res) => {
      res.sendFile(path.join(__dirname, '../dist/index.html'));
    });
    
    console.log("Static file and fallback routes configured.");
    
  } catch (error) {
    console.error("Error setting up server:", error);
  }
}

// --- Server Startup ---

// Handle graceful shutdown
process.on('SIGTERM', async () => {
  console.log('Shutting down frontend server...');
  await backendService.stop();
  process.exit(0);
});

process.on('SIGINT', async () => {
  console.log('Shutting down frontend server...');
  await backendService.stop();
  process.exit(0);
});

app.listen(PORT, "0.0.0.0", async () => {
  console.log(`Kultivate AI Frontend Server listening on port ${PORT}`);
  console.log(`Proxying API requests to: ${BACKEND_URL}`);
  
  // Start the Python backend
  try {
    await backendService.start();
    console.log('Backend service started successfully');
  } catch (error) {
    console.error('Failed to start backend service:', error);
  }
  
  await initializeServer();
});