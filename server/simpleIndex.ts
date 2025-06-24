import express, { type Request, Response, NextFunction } from "express";
import path from "path";
import { spawn, ChildProcess } from 'child_process';

const PORT = parseInt(process.env.PORT ?? "5000", 10);
console.log('Kultivate AI Frontend Server starting on port', PORT);

// Python Backend Service
class PythonBackendService {
  private backendProcess: ChildProcess | null = null;
  private isReady = false;

  start(): void {
    if (this.backendProcess) return;

    console.log('Starting Python backend server...');
    this.backendProcess = spawn('gunicorn', [
      '--worker-class', 'sync',
      '--workers', '1',
      '--bind', '0.0.0.0:8081',
      '--timeout', '120',
      'backend.main_2:app'
    ], {
      cwd: process.cwd(),
      stdio: ['ignore', 'pipe', 'pipe'],
      env: { ...process.env }
    });

    this.backendProcess.stderr?.on('data', (data) => {
      const output = data.toString();
      console.log(`[Backend] ${output.trim()}`);
      if (output.includes('Listening at:')) {
        this.isReady = true;
        console.log('✅ Python backend ready');
      }
    });
  }

  isBackendReady(): boolean {
    return this.isReady;
  }
}

async function startServer() {
  const app = express();
  const pythonBackend = new PythonBackendService();
  
  // Start Python backend
  pythonBackend.start();
  
  // Basic middleware
  app.use(express.json({ limit: '50mb' }));
  app.use(express.urlencoded({ extended: true, limit: '50mb' }));
  app.use(express.static("dist"));

  // Simple auth check endpoint that always returns 401 (unauthenticated)
  app.get('/api/auth/user', (req, res) => {
    res.status(401).json({ message: "Not authenticated" });
  });

  // Proxy for backend API routes
  app.use('/api', (req: Request, res: Response, next: NextFunction) => {
    if (req.path.startsWith('/auth/')) {
      return next();
    }

    if (!pythonBackend.isBackendReady()) {
      return res.status(503).json({ error: 'Backend starting up...' });
    }

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
      .then(response => response.json().then(data => ({ status: response.status, data })))
      .then(({ status, data }) => res.status(status).json(data))
      .catch(error => {
        console.error(`[Proxy Error]:`, error);
        res.status(500).json({ error: 'Backend unavailable' });
      });
  });

  // Serve React app for all other routes
  app.get('*', (req, res) => {
    res.sendFile(path.join(process.cwd(), 'dist', 'index.html'));
  });

  app.listen(PORT, "0.0.0.0", () => {
    console.log(`✅ Kultivate AI serving on port ${PORT}`);
  });
}

startServer().catch(console.error);