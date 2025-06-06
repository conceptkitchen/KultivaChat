import express from "express";
import path from "path";
import { fileURLToPath } from 'url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

const app = express();
app.use(express.json());
app.use(express.urlencoded({ extended: false }));

// Health check
app.get('/health', (req, res) => {
  res.json({ status: 'ok', timestamp: new Date().toISOString() });
});

// Serve the React frontend directly
app.get('/', (req, res) => {
  res.send(`
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Kultivate AI</title>
    <script type="module">
      import { createRoot } from "https://esm.sh/react-dom@18/client";
      import React from "https://esm.sh/react@18";
      
      function App() {
        return React.createElement('div', {
          style: {
            fontFamily: 'Arial, sans-serif',
            padding: '40px',
            maxWidth: '800px',
            margin: '0 auto',
            background: 'white',
            borderRadius: '8px',
            boxShadow: '0 2px 10px rgba(0,0,0,0.1)'
          }
        }, [
          React.createElement('h1', { key: 'title', style: { color: '#333' } }, '🚀 Kultivate AI'),
          React.createElement('p', { key: 'status', style: { color: '#10b981', fontWeight: 'bold' } }, '✓ Frontend is now visible and working!'),
          React.createElement('p', { key: 'desc' }, 'React application is running on port 5000'),
          React.createElement('p', { key: 'next' }, 'Ready to load full application features')
        ]);
      }
      
      const root = createRoot(document.getElementById('root'));
      root.render(React.createElement(App));
    </script>
    <style>
      body { 
        margin: 0; 
        padding: 20px; 
        background: #f5f5f5; 
        font-family: Arial, sans-serif; 
      }
    </style>
</head>
<body>
    <div id="root"></div>
</body>
</html>
  `);
});

const PORT = Number(process.env.PORT) || 5000;
app.listen(PORT, "0.0.0.0", () => {
  console.log(`Frontend visible on port ${PORT}`);
  console.log(`Server successfully bound to 0.0.0.0:${PORT}`);
});