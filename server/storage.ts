import { type Conversation, type IStorage } from "@shared/schema";

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

export const storage = new MemStorage();
