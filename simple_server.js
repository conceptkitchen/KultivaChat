const express = require('express');
const { createProxyMiddleware } = require('http-proxy-middleware');
const path = require('path');

const app = express();
const PORT = 5000;

console.log('Starting simple proxy server...');

// Enable JSON parsing
app.use(express.json());

// API proxy middleware
const apiProxy = createProxyMiddleware('/api', {
  target: 'http://localhost:8081',
  changeOrigin: true,
  logLevel: 'info',
  onError: (err, req, res) => {
    console.error('Proxy error:', err);
    res.status(500).send('Proxy error');
  }
});

app.use('/api', apiProxy);

// Health proxy
app.use('/health', createProxyMiddleware({
  target: 'http://localhost:8081',
  changeOrigin: true
}));

// Static files
app.use(express.static(path.join(__dirname, 'dist')));

// Fallback to React app
app.get('*', (req, res) => {
  res.sendFile(path.join(__dirname, 'dist', 'index.html'));
});

app.listen(PORT, '0.0.0.0', () => {
  console.log(`Frontend server running on port ${PORT}`);
});