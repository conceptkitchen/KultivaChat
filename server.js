import express from 'express';
import { createProxyMiddleware } from 'http-proxy-middleware';
import path from 'path';
import { fileURLToPath } from 'url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

const app = express();
const PORT = process.env.PORT || 5000;

// Proxy API requests to Python backend FIRST (before static files)
app.use('/api', createProxyMiddleware({
  target: 'http://localhost:8081',
  changeOrigin: true,
  logLevel: 'debug'
}));

// Proxy health check to Python backend
app.use('/health', createProxyMiddleware({
  target: 'http://localhost:8081',
  changeOrigin: true
}));

// Serve static files from dist directory (after API proxy)
app.use(express.static(path.join(__dirname, 'dist')));

// Serve React app for all other routes
app.get('*', (req, res) => {
  res.sendFile(path.join(__dirname, 'dist', 'index.html'));
});

app.listen(PORT, '0.0.0.0', () => {
  console.log(`Frontend server running on port ${PORT}`);
});