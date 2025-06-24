import express from "express";
import path from "path";
import { fileURLToPath } from 'url';
import { createProxyMiddleware } from 'http-proxy-middleware';
// Removed Python backend manager - using screen-managed Gunicorn instead

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

// Backend now runs persistently via screen-managed Gunicorn

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
process.on('SIGTERM', () => {
  console.log('Shutting down frontend server...');
  process.exit(0);
});

process.on('SIGINT', () => {
  console.log('Shutting down frontend server...');
  process.exit(0);
});

app.listen(PORT, "0.0.0.0", async () => {
  console.log(`Kultivate AI Frontend Server listening on port ${PORT}`);
  console.log(`Proxying API requests to: ${BACKEND_URL}`);
  await initializeServer();
});