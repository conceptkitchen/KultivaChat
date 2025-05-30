import type { Express } from "express";
import { createServer, type Server } from "http";
import { storage } from "./storage";
import { z } from "zod";
import { v4 as uuidv4 } from "uuid";

import { setupAuth, isAuthenticated } from "./replitAuth";

const messageSchema = z.object({
  conversationId: z.string(),
  content: z.string()
});

const conversationSchema = z.object({
  title: z.string()
});

export async function registerRoutes(app: Express): Promise<Server> {
  // Set up authentication
  await setupAuth(app);



  // User auth route
  app.get('/api/auth/user', isAuthenticated, async (req: any, res) => {
    try {
      const userId = req.user.claims.sub;
      const user = await storage.getUser(userId);
      res.json(user);
    } catch (error) {
      console.error("Error fetching user:", error);
      res.status(500).json({ message: "Failed to fetch user" });
    }
  });

  // API routes - protected by authentication
  app.get("/api/conversations", isAuthenticated, async (req: any, res) => {
    try {
      const userId = req.user.claims.sub;
      const conversations = await storage.getConversations(userId);
      res.json(conversations);
    } catch (error) {
      console.error("Error fetching conversations:", error);
      res.status(500).json({ message: "Failed to fetch conversations" });
    }
  });

  app.get("/api/conversations/:id", isAuthenticated, async (req: any, res) => {
    try {
      const userId = req.user.claims.sub;
      const conversation = await storage.getConversation(req.params.id, userId);

      if (!conversation) {
        return res.status(404).json({ message: "Conversation not found" });
      }

      res.json(conversation);
    } catch (error) {
      console.error("Error fetching conversation:", error);
      res.status(500).json({ message: "Failed to fetch conversation" });
    }
  });

  app.post("/api/conversations", isAuthenticated, async (req: any, res) => {
    try {
      const userId = req.user.claims.sub;
      const data = conversationSchema.parse(req.body);

      const conversation = await storage.createConversation({
        id: uuidv4(),
        userId: userId,
        title: data.title,
        messages: [],
        createdAt: new Date(),
        updatedAt: new Date()
      });

      res.status(201).json(conversation);
    } catch (err) {
      if (err instanceof z.ZodError) {
        return res.status(400).json({ message: "Invalid request data", errors: err.errors });
      }
      console.error("Error creating conversation:", err);
      res.status(500).json({ message: "Failed to create conversation" });
    }
  });

  app.delete("/api/conversations", isAuthenticated, async (req: any, res) => {
    try {
      const userId = req.user.claims.sub;
      await storage.clearConversations(userId);
      res.status(200).json({ message: "All conversations cleared" });
    } catch (error) {
      console.error("Error clearing conversations:", error);
      res.status(500).json({ message: "Failed to clear conversations" });
    }
  });

  app.post("/api/messages", isAuthenticated, async (req: any, res) => {
    try {
      // Ensure user is authenticated and has valid claims
      if (!req.user || !req.user.claims || !req.user.claims.sub) {
        return res.status(401).json({ message: "User not properly authenticated" });
      }

      const userId = req.user.claims.sub;

      // Debug request body
      console.log("Message request body:", JSON.stringify(req.body));

      // Strict duplicate detection - check for exact same message
      const existingConversation = await storage.getConversation(req.body.conversationId, userId);
      if (existingConversation && existingConversation.messages) {
        // Get the last user message
        const lastUserMessage = existingConversation.messages
          .filter(msg => msg.role === "user")
          .pop();

        // If the last user message is identical, it's a definite duplicate
        if (lastUserMessage && lastUserMessage.content === req.body.content) {
          console.log("Exact duplicate message detected, returning existing conversation");
          return res.status(200).json({ 
            message: "Duplicate message ignored",
            conversation: existingConversation
          });
        }
      }

      // Validate the message data
      let data;
      try {
        data = messageSchema.parse(req.body);
      } catch (err) {
        if (err instanceof z.ZodError) {
          console.log("Validation error:", JSON.stringify(err.errors));
          return res.status(400).json({ message: "Invalid request data", errors: err.errors });
        }
        throw err;
      }

      // Verify the conversation exists and belongs to the current user
      const conversation = await storage.getConversation(data.conversationId, userId);
      if (!conversation) {
        return res.status(404).json({ message: "Conversation not found" });
      }

      // Process the user message
      const userMessage = {
        id: uuidv4(),
        role: "user" as const,
        content: data.content,
        timestamp: new Date()
      };

      // Generate AI response based on user message
      let assistantMessage;
      let displays = [];

      try {
        // Call your Python Flask backend for ALL messages
        try {
          console.log("Calling Python backend at http://localhost:8081/api/chat");
          // Forward to Python backend with conversation history
          const pythonResponse = await fetch("http://localhost:8081/api/chat", {
            method: "POST",
            headers: {
              "Content-Type": "application/json",
            },
            body: JSON.stringify({
              message: data.content,
              conversation_history: conversation.messages.map(msg => ({
                role: msg.role,
                content: msg.content,
                timestamp: msg.timestamp
              }))
            }),
          });

          if (!pythonResponse.ok) {
            throw new Error(`Python backend responded with ${pythonResponse.status}`);
          }

          const pythonResult = await pythonResponse.json();
          console.log("Python backend response:", pythonResult);

          assistantMessage = {
            id: uuidv4(),
            role: "assistant" as const,
            content: backendResponse.reply || backendResponse.error || "No response from backend",
            timestamp: new Date(),
            displays: backendResponse.displays || []
          };
        } catch (backendError) {
          console.error("Python backend error:", backendError);
          assistantMessage = {
            id: uuidv4(),
            role: "assistant" as const,
            content: "I'm having trouble connecting to your Python backend. Please make sure your Flask server (main_2.py) is running on port 8081.",
            timestamp: new Date(),
            displays: []
          };
        }
      } catch (error) {
        console.error("Error processing message:", error);
        // General fallback response
        assistantMessage = {
          id: uuidv4(),
          role: "assistant" as const,
          content: "I apologize, but I encountered an issue processing your request. Could you try again or rephrase your question?",
          timestamp: new Date()
        };
      }

      // Add messages to the conversation
      conversation.messages.push(userMessage);
      conversation.messages.push(assistantMessage);
      conversation.updatedAt = new Date();

      // Update conversation title if it's the first user message
      if (conversation.messages.filter((msg: {role: string}) => msg.role === "user").length === 1) {
        conversation.title = data.content.length > 30 
          ? data.content.substring(0, 30) + "..." 
          : data.content;
      }

      await storage.updateConversation(conversation);

      res.status(200).json({ 
        userMessage, 
        assistantMessage 
      });
    } catch (error) {
      if (error instanceof z.ZodError) {
        return res.status(400).json({ message: "Invalid request data", errors: error.errors });
      }
      console.error("Error processing message:", error);
      res.status(500).json({ message: "Failed to process message" });
    }
  });

  const httpServer = createServer(app);
  return httpServer;
}