import express, { type Request, Response, NextFunction } from "express";
import { registerRoutes } from "./routes";
import { registerUnauthedRoutes } from "./routes-unauthed";
import { setupVite, serveStatic } from "./vite";
import { setupAuth } from "./replitAuth";
import { createProxyMiddleware } from 'http-proxy-middleware';

const app = express();
app.use(express.json());
app.use(express.urlencoded({ extended: false }));

// Setup authentication first
setupAuth(app);

// Proxy API calls to Python backend
app.use('/api', createProxyMiddleware({
  target: 'http://127.0.0.1:8081',
  changeOrigin: true,
  pathRewrite: {
    '^/api': '/api'
  }
}));

// Register routes before Vite middleware
registerRoutes(app);
registerUnauthedRoutes(app);

// Setup Vite middleware last
setupVite(app);

const PORT = process.env.PORT || 5000;
app.listen(PORT, "0.0.0.0", () => {
  console.log(`serving on port ${PORT}`);
});