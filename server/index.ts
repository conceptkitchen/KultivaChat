import express from "express";
import path from "path";
import { fileURLToPath } from 'url';
import { spawn } from 'child_process';
import { registerRoutes } from "./routes";

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

const app = express();
app.use(express.json());
app.use(express.urlencoded({ extended: false }));

const PORT = parseInt(process.env.PORT ?? "5000", 10);

let flaskProcess: any = null;

// Start Flask backend server
function startFlaskServer() {
  const backendPath = path.join(__dirname, '../backend');
  flaskProcess = spawn('python', ['main_2.py'], {
    cwd: backendPath,
    stdio: ['ignore', 'pipe', 'pipe']
  });
  
  flaskProcess.stdout?.on('data', (data: Buffer) => {
    console.log(`Flask: ${data.toString().trim()}`);
  });
  
  flaskProcess.stderr?.on('data', (data: Buffer) => {
    console.log(`Flask Error: ${data.toString().trim()}`);
  });
  
  flaskProcess.on('close', (code: number) => {
    console.log(`Flask process exited with code ${code}`);
    if (code !== 0) {
      console.log('Restarting Flask server in 5 seconds...');
      setTimeout(() => startFlaskServer(), 5000);
    }
  });
  
  console.log('Flask backend server starting...');
}

// Health check
app.get('/health', (req, res) => {
  res.json({ status: 'ok', timestamp: new Date().toISOString() });
});

// Setup authentication and routes
async function initializeServer() {
  try {
    // Start Flask backend
    startFlaskServer();
    
    // Register API routes with authentication
    await registerRoutes(app);
    
    // Serve static files from client dist directory
    app.use(express.static(path.join(__dirname, '../dist')));
    
    // Serve React app for all other routes
    app.get('*', (req, res) => {
      res.sendFile(path.join(__dirname, '../dist/index.html'));
    });
    
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