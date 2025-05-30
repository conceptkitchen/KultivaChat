import express, { type Request, Response, NextFunction } from "express";
import { registerRoutes } from "./routes";
import { registerUnauthedRoutes } from "./routes-unauthed";
import { setupVite, serveStatic } from "./vite";
import { setupAuth } from "./replitAuth";
import { createProxyMiddleware } from 'http-proxy-middleware';
import { createServer } from "http";

const app = express();
app.use(express.json());
app.use(express.urlencoded({ extended: false }));

// Setup authentication first
setupAuth(app);

// Register routes before Vite middleware
registerRoutes(app);
registerUnauthedRoutes(app);

// Proxy API calls to Python backend - only for /api/chat route
app.use('/api/chat', createProxyMiddleware({
  target: 'http://127.0.0.1:8081',
  changeOrigin: true,
  timeout: 5000,
  proxyTimeout: 5000,
  onError: (err, req, res) => {
    console.log('Proxy error:', err.message);
    if (!res.headersSent) {
      res.status(500).json({ error: 'Backend service unavailable' });
    }
  },
  onProxyReq: (proxyReq, req, res) => {
    console.log(`Proxying ${req.method} ${req.url} to backend`);
  }
}));

// Create HTTP server for Vite setup
const server = createServer(app);

// Setup Vite middleware last - this will handle serving the React frontend
setupVite(app, server);

const PORT = process.env.PORT || 5000;
server.listen(PORT, "0.0.0.0", () => {
  console.log(`React frontend serving on port ${PORT}`);
  console.log(`Python backend running on port 8081`);
});