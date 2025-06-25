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
    // Always return true if process exists and hasn't been killed
    if (this.backendProcess && !this.backendProcess.killed) {
      return true;
    }
    return this.isReady;
  }

  setReady(): void {
    this.isReady = true;
  }
}

function startServer() {
  const app = express();
  const pythonBackend = new PythonBackendService();
  
  // Start Python backend
  pythonBackend.start();
  
  // Force backend ready state after short delay for startup timing
  setTimeout(() => {
    pythonBackend.setReady();
    console.log('✅ Python backend ready (forced ready state)');
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

  // Handle conversation message endpoint specifically to prevent message duplication
  app.post('/api/conversations/:id/messages', async (req: Request, res: Response) => {
    if (!pythonBackend.isBackendReady()) {
      return res.status(503).json({ error: 'Backend starting up...' });
    }

    const { message } = req.body;
    const conversationId = req.params.id;
    
    const targetUrl = `http://localhost:8081/api/chat`;
    const options: RequestInit = {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
        'Accept': 'application/json',
      },
      body: JSON.stringify({
        message,
        conversation_id: conversationId
      })
    };

    console.log(`[Proxy] POST ${req.path} -> ${targetUrl}`);
    
    try {
      const response = await fetch(targetUrl, options);
      const text = await response.text();
      console.log(`[Proxy] Backend response: ${response.status} ${text.substring(0, 200)}...`);
      
      const data = JSON.parse(text);
      // Only return AI response, not user message to prevent duplication
      res.status(response.status).json({
        reply: data.reply,
        displays: data.displays || []
      });
    } catch (error) {
      console.error(`[Proxy Error]:`, error);
      res.status(503).json({ error: 'Backend connection failed' });
    }
  });

  // API v1 endpoints for external products
  app.post('/api/v1/data/query', async (req: Request, res: Response) => {
    try {
      const { query, credentials } = req.body;
      
      if (!query) {
        return res.status(400).json({ error: 'Query parameter required' });
      }

      if (!pythonBackend.isBackendReady()) {
        return res.status(503).json({ error: 'Backend starting up...' });
      }
      
      const response = await fetch('http://localhost:8081/api/chat', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          message: query,
          api_mode: true,
          credentials: credentials
        })
      });
      
      const data = await response.json();
      
      res.json({
        success: true,
        query: query,
        response: data.reply,
        data: data.displays || [],
        timestamp: new Date().toISOString()
      });
      
    } catch (error) {
      res.status(500).json({
        success: false,
        error: error instanceof Error ? error.message : 'Unknown error',
        timestamp: new Date().toISOString()
      });
    }
  });

  app.post('/api/v1/data/sql', async (req: Request, res: Response) => {
    try {
      const { sql, credentials } = req.body;
      
      if (!sql) {
        return res.status(400).json({ error: 'SQL query required' });
      }

      if (!pythonBackend.isBackendReady()) {
        return res.status(503).json({ error: 'Backend starting up...' });
      }
      
      const response = await fetch('http://localhost:8081/api/execute_sql', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          sql_query: sql,
          credentials: credentials
        })
      });
      
      const result = await response.json();
      
      res.json({
        success: result.status === 'success',
        data: result.data || [],
        error: result.error_message || null,
        rows_returned: result.data ? result.data.length : 0,
        timestamp: new Date().toISOString()
      });
      
    } catch (error) {
      res.status(500).json({
        success: false,
        error: error instanceof Error ? error.message : 'Unknown error',
        timestamp: new Date().toISOString()
      });
    }
  });

  app.get('/api/v1/data/tables', async (req: Request, res: Response) => {
    try {
      const { credentials } = req.body;

      if (!pythonBackend.isBackendReady()) {
        return res.status(503).json({ error: 'Backend starting up...' });
      }
      
      const response = await fetch('http://localhost:8081/api/chat', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          message: "show me all available tables",
          api_mode: true,
          credentials: credentials
        })
      });
      
      const data = await response.json();
      
      res.json({
        success: true,
        tables: data.displays || [],
        total_tables: data.displays ? data.displays.length : 0,
        timestamp: new Date().toISOString()
      });
      
    } catch (error) {
      res.status(500).json({
        success: false,
        error: error.message,
        timestamp: new Date().toISOString()
      });
    }
  });

  // Proxy for backend API routes (simplified) - but skip conversation messages
  app.use('/api', (req: Request, res: Response, next: NextFunction) => {
    // Skip if this is auth, API v1, or conversation messages endpoint
    if (req.path.startsWith('/auth/') || req.path.startsWith('/v1/') || req.path.includes('/conversations/') && req.path.includes('/messages')) {
      return next();
    }

    if (!pythonBackend.isBackendReady()) {
      return res.status(503).json({ error: 'Backend starting up...' });
    }

    const targetUrl = `http://localhost:8081/api${req.path.startsWith('/api') ? req.path.substring(4) : req.path}`;
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

    console.log(`[Proxy] ${req.method} ${req.path} -> ${targetUrl}`);
    
    fetch(targetUrl, options)
      .then(async response => {
        const text = await response.text();
        console.log(`[Proxy] Backend response: ${response.status} ${text.substring(0, 200)}...`);
        try {
          const data = JSON.parse(text);
          return { status: response.status, data };
        } catch (e) {
          console.error(`[Proxy] Backend returned non-JSON:`, text.substring(0, 100));
          return { status: response.status, data: { error: 'Invalid response format', raw: text.substring(0, 500) } };
        }
      })
      .then(({ status, data }) => res.status(status).json(data))
      .catch(error => {
        console.error(`[Proxy Error]:`, error);
        res.status(503).json({ error: 'Backend connection failed', details: error.message });
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