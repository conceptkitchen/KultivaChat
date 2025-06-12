import express from "express";
import path from "path";
import { fileURLToPath } from 'url';
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

const PORT = parseInt(process.env.PORT ?? "5000", 10);

// Serve static files from client dist directory
app.use(express.static(path.join(__dirname, '../dist')));

// Setup authentication and routes
async function initializeServer() {
  try {
    // Register API routes with authentication
    await registerRoutes(app);
    await registerUnauthedRoutes(app);
    
    // Serve React app for all other routes
    app.get('*', (req, res) => {
      res.sendFile(path.join(__dirname, '../dist/index.html'));
    });
    
    console.log("Authentication and routes configured");
  } catch (error) {
    console.error("Error setting up server:", error);
  }
}

app.listen(PORT, "0.0.0.0", async () => {
  console.log(`Kultivate AI serving on port ${PORT}`);
  await initializeServer();
});