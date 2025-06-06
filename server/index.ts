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

// Add basic health check endpoint first
app.get('/health', (req, res) => {
  res.json({ status: 'ok', timestamp: new Date().toISOString() });
});

// Create HTTP server first for proper setup
const server = createServer(app);

// Proxy API calls to Python backend first
app.use('/api/chat', createProxyMiddleware({
  target: 'http://127.0.0.1:8081',
  changeOrigin: true,
  timeout: 5000,
  proxyTimeout: 5000
}));

// Setup Vite middleware last to handle all remaining routes
setupVite(app, server);

const PORT = Number(process.env.PORT) || 5000;
server.listen(PORT, "0.0.0.0", async () => {
  console.log(`Server started on port ${PORT}`);
  console.log(`React frontend serving on port ${PORT}`);
  console.log(`Python backend running on port 8081`);
  
  // Setup authentication after server is listening
  try {
    await setupAuth(app);
    await registerRoutes(app);
    await registerUnauthedRoutes(app);
    console.log("Authentication and routes configured");
  } catch (error) {
    console.error("Error setting up auth:", error);
    // Continue without auth if there's an issue
  }
});