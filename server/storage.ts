import { type Conversation, type IStorage, type User, type InsertUser } from "@shared/schema";
import { DatabaseStorage } from "./database-storage";

export class MemStorage implements IStorage {
  private conversations: Map<string, Conversation>;
  private users: Map<string, User>;

  constructor() {
    this.conversations = new Map();
    this.users = new Map();
  }

  // User operations
  async getUser(id: string): Promise<User | undefined> {
    return this.users.get(id);
  }

  async upsertUser(userData: UpsertUser): Promise<User> {
    const user = {
      ...userData,
      createdAt: this.users.has(userData.id) ? this.users.get(userData.id)!.createdAt : new Date(),
      updatedAt: new Date()
    } as User;
    
    this.users.set(userData.id, user);
    return user;
  }

  // Conversation operations
  async getConversations(userId?: string): Promise<Conversation[]> {
    let conversations = Array.from(this.conversations.values());
    
    if (userId) {
      conversations = conversations.filter(conv => conv.userId === userId);
    }
    
    return conversations.sort((a, b) => 
      new Date(b.updatedAt).getTime() - new Date(a.updatedAt).getTime()
    );
  }

  async getConversation(id: string, userId?: string): Promise<Conversation | undefined> {
    const conversation = this.conversations.get(id);
    
    if (!conversation) {
      return undefined;
    }
    
    if (userId && conversation.userId !== userId) {
      return undefined;
    }
    
    return conversation;
  }

  async createConversation(conversation: Conversation): Promise<Conversation> {
    this.conversations.set(conversation.id, conversation);
    return conversation;
  }

  async updateConversation(conversation: Conversation): Promise<Conversation> {
    this.conversations.set(conversation.id, conversation);
    return conversation;
  }

  async deleteConversation(id: string, userId?: string): Promise<void> {
    const conversation = this.conversations.get(id);
    
    if (!conversation) {
      return;
    }
    
    if (userId && conversation.userId !== userId) {
      throw new Error("Conversation not found or does not belong to user");
    }
    
    this.conversations.delete(id);
  }

  async clearConversations(userId?: string): Promise<void> {
    if (userId) {
      // Delete all conversations for this user
      for (const [id, conversation] of this.conversations.entries()) {
        if (conversation.userId === userId) {
          this.conversations.delete(id);
        }
      }
    } else {
      this.conversations.clear();
    }
  }
}

// For development, you can choose between MemStorage and DatabaseStorage
// Use DatabaseStorage for persistent chat history
import { DatabaseStorage } from "./database-storage";
export const storage = new DatabaseStorage();