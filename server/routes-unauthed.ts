import type { Express } from "express";
import { createServer, type Server } from "http";
import { storage } from "./storage";
import { z } from "zod";
import { v4 as uuidv4 } from "uuid";
// Old MCP services removed - now using Python backend
// Replit auth removed

const messageSchema = z.object({
  conversationId: z.string(),
  content: z.string()
});

export async function registerUnauthedRoutes(app: Express): Promise<void> {
  // No unauthed routes needed - all functionality is in the Python backend
}