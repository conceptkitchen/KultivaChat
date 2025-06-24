import express, { type Request, Response, NextFunction } from "express";
import path from "path";
import { fileURLToPath } from 'url';
import { spawn, ChildProcess } from 'child_process';
import { setupAuth, isAuthenticated } from "./replitAuth";
import { storage } from "./storage";

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

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

    this.backendProcess = spawn('gunicorn', [
      '--worker-class', 'sync',
      '--workers', '1',
      '--bind', '0.0.0.0:8081',
      '--timeout', '120',
      '--keep-alive', '5',
      '--max-requests', '1000',
      '--preload',
      'backend.main_2:app'
    ], {
      cwd: process.cwd(),
      stdio: ['ignore', 'pipe', 'pipe'],
      env: { ...process.env }
    });

    this.backendProcess.stdout?.on('data', (data) => {
      const output = data.toString();
      if (output.includes('Listening at:') || output.includes('Booting worker')) {
        if (!this.isReady) {
          console.log('Backend marked as ready from stdout!');
          this.isReady = true;
        }
      }
    });

    this.backendProcess.stderr?.on('data', (data) => {
      const output = data.toString();
      console.log(`[Backend Error] ${output.trim()}`);
      if (output.includes('Listening at:') || output.includes('Booting worker')) {
        if (!this.isReady) {
          console.log('Backend marked as ready from stderr!');
          this.isReady = true;
        }
      }
    });

    this.backendProcess.on('close', (code) => {
      console.log(`Python backend process exited with code ${code}`);
      this.backendProcess = null;
      this.isReady = false;
      this.isStarting = false;
    });

    this.isStarting = false;
  }

  isBackendReady(): boolean {
    return this.isReady && this.backendProcess !== null;
  }

  stop(): void {
    if (this.backendProcess) {
      this.backendProcess.kill();
      this.backendProcess = null;
      this.isReady = false;
    }
  }
}

// Check if Python backend is running
async function checkBackendHealth(): Promise<boolean> {
  const maxAttempts = 30;
  for (let i = 0; i < maxAttempts; i++) {
    try {
      const response = await fetch("http://localhost:8081/api/hello");
      if (response.ok) {
        console.log("✅ Python backend is ready");
        return true;
      }
    } catch (error) {
      // Backend not ready yet
    }
    
    console.log(`⏳ Waiting for Python backend... (${i + 1}/${maxAttempts})`);
    await new Promise(resolve => setTimeout(resolve, 1000));
  }
  console.error("❌ Python backend failed to start within 30 seconds");
  return false;
}

async function startServer() {
  const app = express();
  
  // Trust proxy for Replit
  app.set('trust proxy', 1);
  
  // Parse JSON bodies
  app.use(express.json({ limit: '50mb' }));
  app.use(express.urlencoded({ extended: true, limit: '50mb' }));
  
  // Serve static files
  app.use(express.static("dist"));

  // Start Python backend
  const pythonBackend = new PythonBackendService();
  pythonBackend.start();
  
  // Wait for backend to be ready
  await checkBackendHealth();

  // Setup Replit authentication
  await setupAuth(app);

  // Auth routes
  app.get('/api/auth/user', isAuthenticated, async (req: any, res) => {
    try {
      const userId = req.user.claims.sub;
      const user = await storage.getUser(userId);
      res.json(user);
    } catch (error) {
      console.error("Error fetching user:", error);
      res.status(500).json({ message: "Failed to fetch user" });
    }
  });

  // Manual proxy implementation for backend API routes (not auth routes)
  app.use('/api', (req: Request, res: Response, next: NextFunction) => {
    // Skip proxy for auth-related routes handled by Replit Auth
    if (req.path.startsWith('/auth/') || req.path === '/login' || req.path === '/logout' || req.path === '/callback' || req.path === '/protected') {
      return next();
    }

    console.log(`[MANUAL PROXY] ${req.method} ${req.path} -> http://localhost:8081${req.path}`);
    console.log(`[MANUAL PROXY] Target URL: http://localhost:8081${req.path}`);
    
    if (req.body && Object.keys(req.body).length > 0) {
      console.log(`[MANUAL PROXY] Sending body:`, req.body);
    }

    // Forward request to Python backend
    const targetUrl = `http://localhost:8081${req.path}`;
    const options: RequestInit = {
      method: req.method,
      headers: {
        'Content-Type': 'application/json',
        'Accept': 'application/json',
      },
    };

    if (req.method !== 'GET' && req.method !== 'HEAD') {
      options.body = JSON.stringify(req.body);
    }

    fetch(targetUrl, options)
      .then(response => {
        console.log(`[MANUAL PROXY] Response: ${response.status} ${response.statusText}`);
        return response.json().then(data => ({ status: response.status, data }));
      })
      .then(({ status, data }) => {
        res.status(status).json(data);
      })
      .catch(error => {
        console.error(`[MANUAL PROXY] Error:`, error);
        res.status(500).json({ error: 'Proxy error', details: error.message });
      });
  });

  // Serve React app for any non-API routes
  app.get('*', (req, res, next) => {
    if (req.path.startsWith('/api/')) {
      return next();
    }
    console.log(`[FRONTEND] Serving React app for: ${req.path}`);
    res.sendFile(path.join(process.cwd(), 'dist', 'index.html'));
  });

  app.listen(PORT, "0.0.0.0", () => {
    console.log(`Kultivate AI serving on port ${PORT}`);
  });
}

startServer().catch(console.error);