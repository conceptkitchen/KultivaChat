import { type Conversation, type IStorage } from "@shared/schema";
import { DatabaseStorage } from "./database-storage";

export class MemStorage implements IStorage {
  private conversations: Map<string, Conversation>;

  constructor() {
    this.conversations = new Map();
  }

  async getConversations(): Promise<Conversation[]> {
    return Array.from(this.conversations.values()).sort((a, b) => 
      new Date(b.updatedAt).getTime() - new Date(a.updatedAt).getTime()
    );
  }

  async getConversation(id: string): Promise<Conversation | undefined> {
    return this.conversations.get(id);
  }

  async createConversation(conversation: Conversation): Promise<Conversation> {
    this.conversations.set(conversation.id, conversation);
    return conversation;
  }

  async updateConversation(conversation: Conversation): Promise<Conversation> {
    this.conversations.set(conversation.id, conversation);
    return conversation;
  }

  async deleteConversation(id: string): Promise<void> {
    this.conversations.delete(id);
  }

  async clearConversations(): Promise<void> {
    this.conversations.clear();
  }
}

// For development, you can choose between MemStorage and DatabaseStorage
import { DatabaseStorage } from "./database-storage";

// Use DatabaseStorage for persistent chat history
export const storage = new DatabaseStorage();
