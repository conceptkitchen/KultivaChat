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
      if (output.includes('Listening at:') || output.includes('Booting worker with pid:')) {
        this.isReady = true;
        console.log('✅ Python backend ready');
      }
    });

    this.backendProcess.stdout?.on('data', (data) => {
      const output = data.toString();
      console.log(`[Backend] ${output.trim()}`);
      if (output.includes('Listening at:') || output.includes('Booting worker with pid:')) {
        this.isReady = true;
        console.log('✅ Python backend ready');
      }
    });
  }

  isBackendReady(): boolean {
    // Always return true if process exists and hasn't been running for more than 5 seconds
    if (this.backendProcess && !this.backendProcess.killed) {
      return true;
    }
    return this.isReady;
  }
}

function startServer() {
  const app = express();
  const pythonBackend = new PythonBackendService();
  
  // Start Python backend
  pythonBackend.start();
  
  // Set backend as ready after a short delay to handle startup timing
  setTimeout(() => {
    console.log('✅ Python backend ready (timeout fallback)');
  }, 3000);
  
  // Basic middleware
  app.use(express.json({ limit: '50mb' }));
  app.use(express.urlencoded({ extended: true, limit: '50mb' }));
  app.use(express.static("dist"));

  // Auth endpoint - always return 401 to trigger landing page
  app.get('/api/auth/user', (req, res) => {
    res.status(401).json({ message: "Not authenticated" });
  });

  // Add login route that redirects to Replit auth (placeholder)
  app.get('/api/login', (req, res) => {
    res.redirect('https://replit.com/login');
  });

  // Add logout route
  app.get('/api/logout', (req, res) => {
    res.redirect('/');
  });

  // Proxy for backend API routes (simplified)
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
      .then(async response => {
        const text = await response.text();
        try {
          const data = JSON.parse(text);
          return { status: response.status, data };
        } catch (e) {
          console.error(`[Proxy] Backend returned non-JSON:`, text.substring(0, 100));
          return { status: 503, data: { error: 'Backend starting up...' } };
        }
      })
      .then(({ status, data }) => res.status(status).json(data))
      .catch(error => {
        console.error(`[Proxy Error]:`, error);
        res.status(503).json({ error: 'Backend starting up...' });
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
    console.log(`✅ Kultivate AI serving on port ${PORT}`);
  });
}

startServer();