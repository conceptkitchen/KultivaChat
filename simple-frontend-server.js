import express from 'express';
import path from 'path';
import { fileURLToPath } from 'url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);
const app = express();

// Serve static files from client directory
app.use(express.static(path.join(__dirname, 'client')));

// Basic HTML page to test frontend visibility
app.get('/', (req, res) => {
  res.send(`
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Kultivate AI</title>
    <style>
        body { 
            font-family: Arial, sans-serif; 
            margin: 0; 
            padding: 20px; 
            background: #f5f5f5; 
        }
        .container { 
            max-width: 800px; 
            margin: 0 auto; 
            background: white; 
            padding: 40px; 
            border-radius: 8px; 
            box-shadow: 0 2px 10px rgba(0,0,0,0.1); 
        }
        h1 { color: #333; }
        .status { color: #10b981; font-weight: bold; }
    </style>
</head>
<body>
    <div class="container">
        <h1>🚀 Kultivate AI Frontend</h1>
        <p class="status">✓ Frontend is now visible and working!</p>
        <p>Server is running on port 5000</p>
        <p>Next: Full React application will load here</p>
    </div>
</body>
</html>
  `);
});

app.get('/health', (req, res) => {
  res.json({ status: 'ok', frontend: 'visible' });
});

const PORT = 5000;
app.listen(PORT, '0.0.0.0', () => {
  console.log(`Frontend visible on port ${PORT}`);
});