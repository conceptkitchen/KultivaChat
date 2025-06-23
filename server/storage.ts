import { type Conversation, type IStorage, type User, type InsertUser } from "@shared/schema";
import { DatabaseStorage } from "./database-storage";

// Use DatabaseStorage for production with PostgreSQL
export const storage = new DatabaseStorage();