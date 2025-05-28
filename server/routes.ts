import type { Express } from "express";
import { createServer, type Server } from "http";
import { storage } from "./storage";
import { z } from "zod";
import { v4 as uuidv4 } from "uuid";
import { getKeboolaData, generateVisualization } from "./services/keboola";
import { generateGeminiResponse } from "./services/gemini";
import { KeboolaMCP } from "./services/keboola-mcp";
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
  
  // Add Keboola test endpoint
  app.get('/api/keboola/test', async (req, res) => {
    try {
      const data = await getKeboolaData();
      res.json({
        success: true,
        message: 'Successfully connected to Keboola',
        data: data.slice(0, 5), // Only return the first 5 records
        total_records: data.length
      });
    } catch (error) {
      console.error('Keboola test error:', error);
      res.status(500).json({
        success: false,
        message: 'Failed to connect to Keboola',
        error: error instanceof Error ? error.message : 'Unknown error'
      });
    }
  });

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
        // Extract previous messages for context
        const previousMessages = conversation.messages.map(msg => ({
          role: msg.role,
          content: msg.content
        })).slice(-10); // Get last 10 messages for context
        
        // Initialize Keboola MCP Server
        let keboolaMCP: KeboolaMCP | null = null;
        try {
          keboolaMCP = new KeboolaMCP();
        } catch (err) {
          console.error("Keboola MCP initialization failed:", err);
        }

        // Enhanced keyword detection for Keboola operations
        const isAskingForVisualization = data.content.toLowerCase().includes("visualization") || 
                                         data.content.toLowerCase().includes("chart") ||
                                         data.content.toLowerCase().includes("graph");
        
        const isAskingForTable = data.content.toLowerCase().includes("table") || 
                                  data.content.toLowerCase().includes("data table") ||
                                  data.content.toLowerCase().includes("show data") ||
                                  data.content.toLowerCase().includes("data is in") ||
                                  data.content.toLowerCase().includes("data from") ||
                                  data.content.toLowerCase().includes("squarespace") ||
                                  data.content.toLowerCase().includes("closeout") ||
                                  data.content.toLowerCase().includes("how many");

        const isAskingForBuckets = data.content.toLowerCase().includes("bucket") ||
                                   data.content.toLowerCase().includes("what buckets") ||
                                   data.content.toLowerCase().includes("list buckets");

        const isAskingForJobs = data.content.toLowerCase().includes("job") ||
                               data.content.toLowerCase().includes("pipeline") ||
                               data.content.toLowerCase().includes("transformation");
                                  
        const isAskingAboutKeboola = data.content.toLowerCase().includes("keboola") ||
                                      data.content.toLowerCase().includes("api") ||
                                      isAskingForTable || isAskingForBuckets || isAskingForJobs;

        // Handle Keboola MCP requests
        if (keboolaMCP && isAskingAboutKeboola) {
          try {
            if (isAskingForBuckets) {
              const buckets = await keboolaMCP.retrieveBuckets();
              const response = keboolaMCP.generateDataResponse(buckets, data.content, 'buckets');
              
              assistantMessage = {
                id: uuidv4(),
                role: "assistant" as const,
                content: response.message,
                timestamp: new Date(),
                displays: response.displays
              };
            } else if (isAskingForJobs) {
              const jobs = await keboolaMCP.retrieveJobs();
              const response = keboolaMCP.generateDataResponse(jobs, data.content, 'jobs');
              
              assistantMessage = {
                id: uuidv4(),
                role: "assistant" as const,
                content: response.message,
                timestamp: new Date(),
                displays: response.displays
              };
            } else if (isAskingForTable) {
              // For table requests, first get available buckets and tables
              const buckets = await keboolaMCP.retrieveBuckets();
              
              if (buckets.length === 0) {
                assistantMessage = {
                  id: uuidv4(),
                  role: "assistant" as const,
                  content: "No data buckets found in your Keboola project. Please check your project configuration and ensure you have data available.",
                  timestamp: new Date(),
                  displays: []
                };
              } else {
                // Look for specific buckets mentioned in the query
                const queryLower = data.content.toLowerCase();
                let targetBucket = buckets[0]; // Default to first bucket
                
                // Check if user mentioned a specific bucket name
                for (const bucket of buckets) {
                  if (queryLower.includes(bucket.name.toLowerCase()) || 
                      queryLower.includes(bucket.id.toLowerCase())) {
                    targetBucket = bucket;
                    break;
                  }
                }
                
                const tables = await keboolaMCP.retrieveBucketTables(targetBucket.id);
                
                if (tables.length > 0) {
                  // If asking about specific data like "closeout" or "squarespace", try to query actual data
                  if (queryLower.includes("closeout") || queryLower.includes("how many")) {
                    try {
                      // Try to query the first table for sample data
                      const sampleQuery = `SELECT * FROM ${tables[0].id} LIMIT 10`;
                      const queryResults = await keboolaMCP.queryTable(sampleQuery);
                      
                      assistantMessage = {
                        id: uuidv4(),
                        role: "assistant" as const,
                        content: `Here's a sample of data from your "${tables[0].name}" table in bucket "${targetBucket.name}":`,
                        timestamp: new Date(),
                        displays: [{
                          type: "table",
                          title: `Sample Data: ${tables[0].name}`,
                          content: queryResults
                        }]
                      };
                    } catch (queryError) {
                      // If query fails, show table structure instead
                      const tableDetail = await keboolaMCP.getTableDetail(tables[0].id);
                      assistantMessage = {
                        id: uuidv4(),
                        role: "assistant" as const,
                        content: `Found ${tables.length} tables in bucket "${targetBucket.name}". Here's the structure of "${tableDetail.name}":`,
                        timestamp: new Date(),
                        displays: [{
                          type: "table",
                          title: `Table Structure: ${tableDetail.name}`,
                          content: [{
                            'Table ID': tableDetail.id,
                            'Rows': tableDetail.rowsCount,
                            'Columns': tableDetail.columns.join(', '),
                            'Size': `${Math.round(tableDetail.dataSizeBytes / 1024 / 1024 * 100) / 100} MB`,
                            'Last Updated': new Date(tableDetail.lastChangeDate).toLocaleDateString()
                          }]
                        }]
                      };
                    }
                  } else {
                    // Show all tables in the bucket
                    const tablesList = tables.map(table => ({
                      'Table Name': table.name,
                      'Table ID': table.id,
                      'Rows': table.rowsCount,
                      'Columns': table.columns.length,
                      'Size (MB)': Math.round(table.dataSizeBytes / 1024 / 1024 * 100) / 100,
                      'Last Updated': new Date(table.lastChangeDate).toLocaleDateString()
                    }));
                    
                    assistantMessage = {
                      id: uuidv4(),
                      role: "assistant" as const,
                      content: `Found ${tables.length} tables in bucket "${targetBucket.name}":`,
                      timestamp: new Date(),
                      displays: [{
                        type: "table",
                        title: `Tables in ${targetBucket.name}`,
                        content: tablesList
                      }]
                    };
                  }
                } else {
                  assistantMessage = {
                    id: uuidv4(),
                    role: "assistant" as const,
                    content: `Found ${buckets.length} buckets but no tables in "${targetBucket.name}". You may need to load data into this bucket first.`,
                    timestamp: new Date(),
                    displays: []
                  };
                }
              }
            } else if (isAskingForVisualization) {
              // For visualization, we'd need to implement chart generation
              assistantMessage = {
                id: uuidv4(),
                role: "assistant" as const,
                content: "Visualization features are being set up. For now, I can show you data tables and bucket information from your Keboola project.",
                timestamp: new Date(),
                displays: []
              };
            } else {
              // General Keboola query - show project overview
              const buckets = await keboolaMCP.retrieveBuckets();
              const response = keboolaMCP.generateDataResponse(buckets, data.content, 'buckets');
              
              assistantMessage = {
                id: uuidv4(),
                role: "assistant" as const,
                content: `Here's an overview of your Keboola project:\n\n${response.message}`,
                timestamp: new Date(),
                displays: response.displays
              };
            }
          } catch (mcpError) {
            console.error("Keboola MCP error:", mcpError);
            assistantMessage = {
              id: uuidv4(),
              role: "assistant" as const,
              content: "I'm having trouble connecting to your Keboola data right now. This could be due to authentication issues or network connectivity. Please check your Keboola credentials and try again.",
              timestamp: new Date(),
              displays: []
            };
          }
        } else {
          // For all other requests, use Gemini model
          try {
            // Get response from Gemini
            const geminiResponse = await generateGeminiResponse(data.content, previousMessages);
            
            assistantMessage = {
              id: uuidv4(),
              role: "assistant" as const,
              content: geminiResponse,
              timestamp: new Date(),
              displays: []
            };
          } catch (error) {
            console.error("Error generating Gemini response:", error);
            // Fallback if Gemini fails
            assistantMessage = {
              id: uuidv4(),
              role: "assistant" as const,
              content: "I'm sorry, but I'm having trouble connecting to my AI service right now. Please try again in a few moments.",
              timestamp: new Date()
            };
          }
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
