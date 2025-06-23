import express, { type Express } from "express";
import fs from "fs";
import path from "path";
import { createServer as createViteServer, createLogger } from "vite";
import { type Server } from "http";
import viteConfig from "../vite.config";
import { nanoid } from "nanoid";

const viteLogger = createLogger();

export function log(message: string, source = "express") {
  const formattedTime = new Date().toLocaleTimeString("en-US", {
    hour: "numeric",
    minute: "2-digit",
    second: "2-digit",
    hour12: true,
  });

  console.log(`${formattedTime} [${source}] ${message}`);
}

export async function setupVite(app: express.Express, server?: Server) {
  if (process.env.NODE_ENV === "production") {
    console.log("Production mode: serving static React build");
    serveStatic(app);
  } else {
    console.log("Development mode: setting up Vite dev server");
    const { createServer: createViteServer } = await import("vite");
    const vite = await createViteServer({
      server: { 
        middlewareMode: true,
        hmr: server ? {
          server: server,
          port: 24678
        } : false
      },
      appType: "spa",
      clearScreen: false,
      optimizeDeps: {
        include: ["wouter", "@tanstack/react-query"]
      }
    });

    app.use(vite.ssrFixStacktrace);
    app.use(vite.middlewares);

    // Catch-all handler for SPA routing
    app.use("*", async (req, res, next) => {
      const url = req.originalUrl;
      try {
        let template = await vite.transformIndexHtml(url, `
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="UTF-8" />
    <link rel="icon" type="image/svg+xml" href="/vite.svg" />
    <meta name="viewport" content="width=device-width, initial-scale=1.0" />
    <title>Kultivate AI</title>
  </head>
  <body>
    <div id="root"></div>
    <script type="module" src="/src/main.tsx"></script>
  </body>
</html>
        `);
        res.status(200).set({ "Content-Type": "text/html" }).end(template);
      } catch (e) {
        vite.ssrFixStacktrace(e as Error);
        next(e);
      }
    });
  }
}

export function serveStatic(app: Express) {
  const distPath = path.resolve(import.meta.dirname, "..", "dist");

  if (!fs.existsSync(distPath)) {
    throw new Error(
      `Could not find the build directory: ${distPath}, make sure to build the client first`,
    );
  }

  // Serve static files with proper headers
  app.use(express.static(distPath, {
    setHeaders: (res, path) => {
      if (path.endsWith('.html')) {
        res.setHeader('Content-Type', 'text/html');
      }
    }
  }));

  // Catch-all handler for SPA routing - serve React app for all non-API routes
  app.get("*", (req, res) => {
    // Don't serve React app for API routes
    if (req.path.startsWith('/api')) {
      return res.status(404).json({ error: 'API endpoint not found' });
    }
    
    const indexPath = path.resolve(distPath, "index.html");
    res.setHeader('Content-Type', 'text/html');
    res.sendFile(indexPath);
  });
}