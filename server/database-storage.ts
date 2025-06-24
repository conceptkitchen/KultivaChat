import { db, pool } from "./db";
import { type Conversation, type IStorage, conversations, messages, users, type User, type UpsertUser } from "@shared/schema";
import { eq, desc, and } from "drizzle-orm";
import session from "express-session";
import connectPg from "connect-pg-simple";

const PostgresSessionStore = connectPg(session);

export class DatabaseStorage implements IStorage {
  sessionStore: any;

  constructor() {
    this.sessionStore = new PostgresSessionStore({ 
      pool, 
      createTableIfMissing: true 
    });
  }

  // User operations
  // (IMPORTANT) these user operations are mandatory for Replit Auth.

  async getUser(id: string): Promise<User | undefined> {
    try {
      const [user] = await db.select().from(users).where(eq(users.id, id));
      return user;
    } catch (error) {
      console.error("Error fetching user:", error);
      return undefined;
    }
  }

  async upsertUser(userData: UpsertUser): Promise<User> {
    try {
      const [user] = await db
        .insert(users)
        .values(userData)
        .onConflictDoUpdate({
          target: users.id,
          set: {
            ...userData,
            updatedAt: new Date(),
          },
        })
        .returning();
      return user;
    } catch (error) {
      console.error("Error upserting user:", error);
      throw error;
    }
  }

  // Get all conversations for a specific user, or all conversations if userId is not provided
  async getConversations(userId?: string): Promise<Conversation[]> {
    try {
      const query = userId 
        ? db.select().from(conversations).where(eq(conversations.userId, userId)).orderBy(desc(conversations.updatedAt))
        : db.select().from(conversations).orderBy(desc(conversations.updatedAt));
      
      const dbConversations = await query;
      
      const result: Conversation[] = [];
      
      for (const conversation of dbConversations) {
        const messagesList = await db
          .select()
          .from(messages)
          .where(eq(messages.conversationId, conversation.id))
          .orderBy(messages.timestamp);
        
        result.push({
          ...conversation,
          messages: messagesList.map(msg => ({
            id: msg.id,
            role: msg.role as "user" | "assistant",
            content: msg.content,
            displays: msg.displays as any[] | undefined,
            timestamp: msg.timestamp
          }))
        });
      }
      
      return result;
    } catch (error) {
      console.error("Error fetching conversations:", error);
      return [];
    }
  }

  async getConversation(id: string, userId?: string): Promise<Conversation | undefined> {
    try {
      // If userId is provided, ensure the conversation belongs to the user
      const query = userId
        ? db.select().from(conversations).where(and(eq(conversations.id, id), eq(conversations.userId, userId)))
        : db.select().from(conversations).where(eq(conversations.id, id));
        
      const [conversation] = await query;
      
      if (!conversation) {
        return undefined;
      }
      
      const messagesList = await db
        .select()
        .from(messages)
        .where(eq(messages.conversationId, id))
        .orderBy(messages.timestamp);
      
      return {
        ...conversation,
        messages: messagesList.map(msg => ({
          id: msg.id,
          role: msg.role as "user" | "assistant",
          content: msg.content,
          displays: msg.displays as any[] | undefined,
          timestamp: msg.timestamp
        }))
      };
    } catch (error) {
      console.error("Error fetching conversation:", error);
      return undefined;
    }
  }

  async createConversation(conversation: Conversation): Promise<Conversation> {
    try {
      await db.insert(conversations).values({
        id: conversation.id,
        userId: conversation.userId,
        title: conversation.title,
        createdAt: conversation.createdAt,
        updatedAt: conversation.updatedAt
      });
      
      for (const msg of conversation.messages) {
        await db.insert(messages).values({
          id: msg.id,
          conversationId: conversation.id,
          role: msg.role,
          content: msg.content,
          displays: msg.displays as any,
          timestamp: msg.timestamp
        });
      }
      
      return conversation;
    } catch (error) {
      console.error("Error creating conversation:", error);
      throw error;
    }
  }

  async updateConversation(conversation: Conversation): Promise<Conversation> {
    try {
      await db
        .update(conversations)
        .set({
          title: conversation.title,
          updatedAt: conversation.updatedAt
        })
        .where(eq(conversations.id, conversation.id));
      
      // Get existing messages
      const existingMessages = await db
        .select()
        .from(messages)
        .where(eq(messages.conversationId, conversation.id));
      
      const existingIds = new Set(existingMessages.map(m => m.id));
      
      // Insert new messages
      for (const msg of conversation.messages) {
        if (!existingIds.has(msg.id)) {
          await db.insert(messages).values({
            id: msg.id,
            conversationId: conversation.id,
            role: msg.role,
            content: msg.content,
            displays: msg.displays as any,
            timestamp: msg.timestamp
          });
        }
      }
      
      return conversation;
    } catch (error) {
      console.error("Error updating conversation:", error);
      throw error;
    }
  }

  async deleteConversation(id: string, userId?: string): Promise<void> {
    try {
      // If userId is provided, ensure the conversation belongs to the user before deleting
      if (userId) {
        const [conversation] = await db
          .select()
          .from(conversations)
          .where(and(eq(conversations.id, id), eq(conversations.userId, userId)));
          
        if (!conversation) {
          throw new Error("Conversation not found or does not belong to user");
        }
      }
      
      // Delete associated messages first
      await db.delete(messages).where(eq(messages.conversationId, id));
      // Then delete the conversation
      await db.delete(conversations).where(eq(conversations.id, id));
    } catch (error) {
      console.error("Error deleting conversation:", error);
      throw error;
    }
  }

  async clearConversations(userId?: string): Promise<void> {
    try {
      if (userId) {
        // Get all conversation IDs for this user
        const userConversations = await db
          .select({ id: conversations.id })
          .from(conversations)
          .where(eq(conversations.userId, userId));
        
        const conversationIds = userConversations.map(c => c.id);
        
        // Delete messages for these conversations
        for (const convId of conversationIds) {
          await db.delete(messages).where(eq(messages.conversationId, convId));
        }
        
        // Delete the conversations
        await db.delete(conversations).where(eq(conversations.userId, userId));
      } else {
        // Delete all messages and conversations
        await db.delete(messages);
        await db.delete(conversations);
      }
    } catch (error) {
      console.error("Error clearing conversations:", error);
      throw error;
    }
  }
}