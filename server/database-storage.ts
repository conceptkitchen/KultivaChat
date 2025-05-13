import { db } from "./db";
import { type Conversation, type IStorage, conversations, messages } from "@shared/schema";
import { eq, desc } from "drizzle-orm";

export class DatabaseStorage implements IStorage {
  async getConversations(): Promise<Conversation[]> {
    const dbConversations = await db.select().from(conversations).orderBy(desc(conversations.updatedAt));
    
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
  }

  async getConversation(id: string): Promise<Conversation | undefined> {
    const [conversation] = await db
      .select()
      .from(conversations)
      .where(eq(conversations.id, id));
    
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
  }

  async createConversation(conversation: Conversation): Promise<Conversation> {
    await db.insert(conversations).values({
      id: conversation.id,
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
  }

  async updateConversation(conversation: Conversation): Promise<Conversation> {
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
  }

  async deleteConversation(id: string): Promise<void> {
    // Delete associated messages first
    await db.delete(messages).where(eq(messages.conversationId, id));
    // Then delete the conversation
    await db.delete(conversations).where(eq(conversations.id, id));
  }

  async clearConversations(): Promise<void> {
    // Delete all messages and conversations
    await db.delete(messages);
    await db.delete(conversations);
  }
}