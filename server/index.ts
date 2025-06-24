import express from "express";
import path from "path";
import { fileURLToPath } from 'url';
import { createProxyMiddleware } from 'http-proxy-middleware';
import { spawn, ChildProcess } from 'child_process';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

const app = express();

// CRITICAL: Parse JSON bodies BEFORE proxy middleware
app.use(express.json({ limit: '50mb' }));
app.use(express.urlencoded({ extended: true, limit: '50mb' }));

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
          console.log('Backend marked as ready!');
        }
      });

      this.backendProcess.stderr?.on('data', (data) => {
        const output = data.toString();
        console.error('[Backend Error]', output.trim());
        
        // Also check stderr for readiness indicators
        if (output.includes('Listening at:') || output.includes('Gemini GenerateContentConfig with tools created successfully')) {
          this.isReady = true;
          console.log('Backend marked as ready from stderr!');
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

// --- Manual API Proxy (Direct Implementation) ---
app.all('/api/*', async (req, res) => {
  try {
    console.log(`[MANUAL PROXY] ${req.method} ${req.url} -> ${BACKEND_URL}${req.url}`);
    
    // Ensure backend is ready
    if (!backendService.isBackendReady()) {
      return res.status(503).json({ error: 'Backend service unavailable' });
    }

    const targetUrl = `${BACKEND_URL}${req.url}`;
    console.log(`[MANUAL PROXY] Target URL: ${targetUrl}`);
    
    // Create fetch request
    const fetchOptions: any = {
      method: req.method,
      headers: {
        'Content-Type': 'application/json',
        'Accept': 'application/json'
      }
    };

    // Add body for POST/PUT requests
    if ((req.method === 'POST' || req.method === 'PUT') && req.body) {
      fetchOptions.body = JSON.stringify(req.body);
      console.log(`[MANUAL PROXY] Sending body:`, req.body);
    }

    // Make the request to backend
    const response = await fetch(targetUrl, fetchOptions);
    const data = await response.text();
    
    console.log(`[MANUAL PROXY] Response: ${response.status} ${response.statusText}`);
    
    // Set response headers
    response.headers.forEach((value, key) => {
      res.setHeader(key, value);
    });
    
    // Send response
    res.status(response.status).send(data);
    
  } catch (error) {
    console.error('[MANUAL PROXY] Error:', error);
    res.status(500).json({ error: 'Proxy error', message: error.message });
  }
});

// --- Frontend Server Logic ---

// Health check
app.get('/health', (req, res) => {
  res.json({ status: 'ok', timestamp: new Date().toISOString() });
});

// Serve static files from the client's build directory BEFORE fallback
app.use(express.static(path.join(__dirname, '../dist')));

// For any other route (non-API), serve the React app's index.html file.
// This is necessary for client-side routing to work correctly.
app.get('*', (req, res) => {
  console.log(`[FRONTEND] Serving React app for: ${req.url}`);
  res.sendFile(path.join(__dirname, '../dist/index.html'));
});

async function initializeServer() {
  try {
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