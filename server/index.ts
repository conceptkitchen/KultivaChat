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
      // Production: serve React app directly from client directory
      console.log("Production mode: serving React app from client");
      app.use(express.static(path.join(__dirname, '../client')));
      app.use('/src', express.static(path.join(__dirname, '../client/src')));
      app.use('/node_modules', express.static(path.join(__dirname, '../node_modules')));
      
      // Handle React Router routes - serve index.html for all non-API routes
      app.get('*', (req, res) => {
        res.sendFile(path.join(__dirname, '../index.html'));
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