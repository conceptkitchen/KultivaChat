import type { Express } from "express";
import { createServer, type Server } from "http";
import { storage } from "./storage";
import { z } from "zod";
import { v4 as uuidv4 } from "uuid";
import { getKeboolaData, generateVisualization } from "./services/keboola";
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
    } catch (error) {
      if (error instanceof z.ZodError) {
        return res.status(400).json({ message: "Invalid request data", errors: error.errors });
      }
      console.error("Error creating conversation:", error);
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
      const userId = req.user.claims.sub;
      const data = messageSchema.parse(req.body);
      
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

      // Keywords to look for in the user's message
      const isAskingForVisualization = data.content.toLowerCase().includes("visualization") || 
                                       data.content.toLowerCase().includes("chart") ||
                                       data.content.toLowerCase().includes("graph");
      
      const isAskingForTable = data.content.toLowerCase().includes("table") || 
                                data.content.toLowerCase().includes("data table") ||
                                data.content.toLowerCase().includes("show data");
      
      const isAskingForCode = data.content.toLowerCase().includes("code") || 
                               data.content.toLowerCase().includes("script") ||
                               data.content.toLowerCase().includes("implementation");
      
      const isAskingAboutKeboola = data.content.toLowerCase().includes("keboola") ||
                                    data.content.toLowerCase().includes("api");

      try {
        // Fetch data from Keboola if requested
        let keboolaData = null;
        if (isAskingAboutKeboola || isAskingForVisualization || isAskingForTable) {
          try {
            keboolaData = await getKeboolaData();
          } catch (error) {
            console.error("Error fetching Keboola data:", error);
          }
        }

        // Build appropriate response
        if (isAskingForVisualization && keboolaData) {
          const visualizationResponse = await generateVisualization(keboolaData, data.content);
          
          assistantMessage = {
            id: uuidv4(),
            role: "assistant" as const,
            content: visualizationResponse.message,
            timestamp: new Date(),
            displays: visualizationResponse.displays
          };
        } else if (isAskingForTable && keboolaData) {
          // Process data for table display
          assistantMessage = {
            id: uuidv4(),
            role: "assistant" as const,
            content: "Here's the data table you requested:",
            timestamp: new Date(),
            displays: [
              {
                type: "table",
                title: "Data Table",
                content: keboolaData.slice(0, 10) // Limit to 10 rows for display
              }
            ]
          };
        } else if (isAskingForCode) {
          // Generate code sample based on request
          const codeSnippet = `// Example code for ${data.content.includes("Keboola") ? "Keboola API integration" : "data processing"}
const fetchData = async () => {
  const API_URL = 'https://connection.keboola.com/v2/storage';
  const API_KEY = process.env.KEBOOLA_API_KEY;
  
  try {
    const response = await fetch(\`\${API_URL}/tables/your-table-id/export\`, {
      method: 'GET',
      headers: {
        'X-StorageApi-Token': API_KEY,
        'Content-Type': 'application/json'
      }
    });
    
    if (!response.ok) {
      throw new Error(\`API error: \${response.status}\`);
    }
    
    const data = await response.json();
    return data;
  } catch (error) {
    console.error('Error fetching data:', error);
    throw error;
  }
};`;

          assistantMessage = {
            id: uuidv4(),
            role: "assistant" as const,
            content: "Here's a code example that might help you:",
            timestamp: new Date(),
            displays: [
              {
                type: "code",
                title: "Code Example",
                language: "javascript",
                content: codeSnippet
              }
            ]
          };
        } else {
          // General response
          assistantMessage = {
            id: uuidv4(),
            role: "assistant" as const,
            content: `I'd be happy to help with that. Could you provide more details about what you're looking for? I can help with data visualization, Keboola API integration, code generation, or creating documentation.`,
            timestamp: new Date()
          };
        }
      } catch (error) {
        // Fallback response if something goes wrong
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
