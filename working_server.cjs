const express = require('express');
const { createProxyMiddleware } = require('http-proxy-middleware');
const path = require('path');

const app = express();
const PORT = 5000;

console.log('Starting proxy server on port 5000...');

// API proxy - routes /api/* to backend on 8081
app.use('/api', createProxyMiddleware({
  target: 'http://localhost:8081',
  changeOrigin: true,
  onProxyReq: (proxyReq, req, res) => {
    console.log(`Proxying: ${req.method} ${req.url} -> http://localhost:8081${req.url}`);
  },
  onError: (err, req, res) => {
    console.error('Proxy error:', err.message);
    res.status(500).send('Proxy error');
  }
}));

// Additional proxies for frontend API calls that don't use /api prefix
app.use('/user', createProxyMiddleware({
  target: 'http://localhost:8081',
  changeOrigin: true,
  pathRewrite: { '^/user': '/api/auth/me' },
  onProxyReq: (proxyReq, req, res) => {
    console.log(`User proxy: ${req.method} ${req.url} -> http://localhost:8081/api/auth/me`);
  }
}));

app.use('/conversations', createProxyMiddleware({
  target: 'http://localhost:8081',
  changeOrigin: true,
  pathRewrite: { '^/conversations': '/api/conversations' },
  onProxyReq: (proxyReq, req, res) => {
    console.log(`Conversations proxy: ${req.method} ${req.url} -> http://localhost:8081/api/conversations`);
  }
}));

// Serve static files
app.use(express.static(path.join(__dirname, 'dist')));

// Fallback for React routes
app.get('*', (req, res) => {
  res.sendFile(path.join(__dirname, 'dist', 'index.html'));
});

app.listen(PORT, '0.0.0.0', () => {
  console.log(`Frontend server running on port ${PORT}`);
});