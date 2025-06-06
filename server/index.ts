import express from "express";
import path from "path";
import { fileURLToPath } from 'url';
import { setupAuth } from "./replitAuth";
import { registerRoutes } from "./routes";
import { registerUnauthedRoutes } from "./routes-unauthed";

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

const app = express();
app.use(express.json());
app.use(express.urlencoded({ extended: false }));

// Health check
app.get('/health', (req, res) => {
  res.json({ status: 'ok', timestamp: new Date().toISOString() });
});

// Serve the login page
app.get('/', (req, res) => {
  res.send(`<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Kultivate AI Assistant</title>
    <style>
        body {
            margin: 0;
            font-family: 'Inter', -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
            background: #f9fafb;
            display: flex;
            align-items: center;
            justify-content: center;
            min-height: 100vh;
            padding: 20px;
        }
        .login-card {
            background: white;
            padding: 2rem;
            border-radius: 8px;
            box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
            text-align: center;
            max-width: 400px;
            width: 100%;
        }
        .logo {
            width: 48px;
            height: 48px;
            background: #eab308;
            border-radius: 6px;
            display: flex;
            align-items: center;
            justify-content: center;
            margin: 0 auto 1rem;
        }
        .title {
            font-size: 1.5rem;
            font-weight: 700;
            margin: 0 0 0.5rem;
            color: #1f2937;
        }
        .subtitle {
            color: #6b7280;
            margin: 0 0 2rem;
            line-height: 1.5;
        }
        .login-btn {
            background: #eab308;
            color: white;
            padding: 10px 20px;
            border: none;
            border-radius: 6px;
            font-weight: 500;
            text-decoration: none;
            display: inline-block;
            cursor: pointer;
            transition: background-color 0.15s;
        }
        .login-btn:hover {
            background: #ca8a04;
        }
        .highlight {
            color: #eab308;
        }
    </style>
</head>
<body>
    <div class="login-card">
        <div class="logo">
            <svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="white" stroke-width="2.5" stroke-linecap="round" stroke-linejoin="round">
                <path d="M12 8V4H8"></path>
                <rect width="16" height="12" x="4" y="8" rx="2"></rect>
                <path d="M2 14h2"></path>
                <path d="M20 14h2"></path>
                <path d="M15 13v2"></path>
                <path d="M9 13v2"></path>
            </svg>
        </div>
        <h1 class="title">
            Kultivate <span class="highlight">AI</span>
        </h1>
        <p class="subtitle">
            Your AI-powered data integration platform for intelligent exploration and visualization
        </p>
        <a href="/api/login" class="login-btn">
            Log In to Continue
        </a>
    </div>
</body>
</html>`);
});

// Basic authentication routes
app.get('/api/login', (req, res) => {
  res.redirect('/?auth=required');
});

const PORT = Number(process.env.PORT) || 5000;

// Setup authentication and routes
async function initializeServer() {
  try {
    // Setup authentication system
    await setupAuth(app);
    
    // Register API routes
    await registerRoutes(app);
    await registerUnauthedRoutes(app);
    
    console.log("Authentication and routes configured");
  } catch (error) {
    console.error("Error setting up auth:", error);
  }
}

app.listen(PORT, "0.0.0.0", async () => {
  console.log(`Kultivate AI serving on port ${PORT}`);
  await initializeServer();
});