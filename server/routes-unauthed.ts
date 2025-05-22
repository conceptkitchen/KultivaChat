import type { Express } from "express";
import { createServer, type Server } from "http";
import { storage } from "./storage";
import { z } from "zod";
import { v4 as uuidv4 } from "uuid";
import { getKeboolaData } from "./services/keboola";
import { generateGeminiResponse } from "./services/gemini";
import { setupAuth } from "./replitAuth";

const messageSchema = z.object({
  conversationId: z.string(),
  content: z.string()
});

export async function registerUnauthedRoutes(app: Express): Promise<void> {
  // Testing route for gemini integration, no authentication required
  app.post("/api/gemini-test", async (req, res) => {
    try {
      const data = messageSchema.parse(req.body);
      
      // Generate a response using Gemini
      try {
        const geminiResponse = await generateGeminiResponse(data.content, []);
        
        const assistantMessage = {
          id: uuidv4(),
          role: "assistant",
          content: geminiResponse.content,
          timestamp: new Date(),
          displays: geminiResponse.displays
        };
        
        res.status(200).json({ 
          assistantMessage 
        });
      } catch (err) {
        console.error("Error generating Gemini response:", err);
        res.status(500).json({ 
          message: "Failed to generate response from Gemini" 
        });
      }
    } catch (err) {
      if (err instanceof z.ZodError) {
        return res.status(400).json({ message: "Invalid request data", errors: err.errors });
      }
      console.error("Error processing message:", err);
      res.status(500).json({ message: "Failed to process message" });
    }
  });
}