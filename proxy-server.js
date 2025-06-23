import express from 'express';
import { createProxyMiddleware } from 'http-proxy-middleware';
import path from 'path';
import { fileURLToPath } from 'url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

const app = express();

// Serve static files from dist folder
app.use(express.static(path.join(__dirname, 'dist')));

// Proxy all API calls to the Python backend
app.use('/api', createProxyMiddleware({
  target: 'http://localhost:8081',
  changeOrigin: true,
  onError: (err, req, res) => {
    console.error('Proxy Error:', err.message);
    res.status(500).json({ error: 'Backend connection failed' });
  },
  onProxyReq: (proxyReq, req, res) => {
    console.log(`Proxying ${req.method} ${req.url} to backend`);
  }
}));

// Serve React app for all other routes
app.get('*', (req, res) => {
  res.sendFile(path.join(__dirname, 'dist', 'index.html'));
});

const PORT = 5000;
app.listen(PORT, '0.0.0.0', () => {
  console.log(`Kultivate AI serving on port ${PORT}`);
  console.log('Authentication and routes configured');
});