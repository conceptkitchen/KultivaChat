import {
  pgTable,
  text,
  varchar,
  timestamp,
  jsonb,
  index,
} from "drizzle-orm/pg-core";
import { createInsertSchema } from "drizzle-zod";
import { z } from "zod";

// Session storage table.
// (IMPORTANT) This table is mandatory for Replit Auth, don't drop it.
export const sessions = pgTable(
  "sessions",
  {
    sid: varchar("sid").primaryKey(),
    sess: jsonb("sess").notNull(),
    expire: timestamp("expire").notNull(),
  },
  (table) => [index("IDX_session_expire").on(table.expire)],
);

// User storage table.
// (IMPORTANT) This table is mandatory for Replit Auth, don't drop it.
export const users = pgTable("users", {
  id: varchar("id").primaryKey().notNull(),
  email: varchar("email").unique(),
  firstName: varchar("first_name"),
  lastName: varchar("last_name"),
  profileImageUrl: varchar("profile_image_url"),
  createdAt: timestamp("created_at").defaultNow(),
  updatedAt: timestamp("updated_at").defaultNow(),
});



export const messages = pgTable("messages", {
  id: text("id").primaryKey(),
  conversationId: text("conversation_id").notNull(),
  role: text("role").notNull(),
  content: text("content").notNull(),
  displays: jsonb("displays"),
  timestamp: timestamp("timestamp").notNull().defaultNow()
});

export const conversations = pgTable("conversations", {
  id: text("id").primaryKey(),
  userId: varchar("user_id").references(() => users.id),
  title: text("title").notNull(),
  createdAt: timestamp("created_at").notNull().defaultNow(),
  updatedAt: timestamp("updated_at").notNull().defaultNow()
});

export const insertMessageSchema = createInsertSchema(messages).omit({
  id: true,
  timestamp: true
});

export const insertConversationSchema = createInsertSchema(conversations).omit({
  id: true,
  createdAt: true,
  updatedAt: true
});

export const insertUserSchema = createInsertSchema(users).omit({
  id: true,
  createdAt: true,
  updatedAt: true
});

export type InsertMessage = z.infer<typeof insertMessageSchema>;
export type Message = typeof messages.$inferSelect;

export type InsertConversation = z.infer<typeof insertConversationSchema>;
export type Conversation = typeof conversations.$inferSelect & {
  messages: Array<{
    id: string;
    role: "user" | "assistant";
    content: string;
    displays?: any[];
    timestamp: Date;
  }>;
};

export type UpsertUser = typeof users.$inferInsert;
export type User = typeof users.$inferSelect;
export type InsertUser = z.infer<typeof insertUserSchema>;

// Storage interface types for database and memory storage
export interface IStorage {
  // User operations
  // (IMPORTANT) these user operations are mandatory for Replit Auth.
  getUser(id: string): Promise<User | undefined>;
  upsertUser(user: UpsertUser): Promise<User>;
  
  // Conversation operations
  getConversations(userId?: string): Promise<Conversation[]>;
  getConversation(id: string, userId?: string): Promise<Conversation | undefined>;
  createConversation(conversation: Conversation): Promise<Conversation>;
  updateConversation(conversation: Conversation): Promise<Conversation>;
  deleteConversation(id: string, userId?: string): Promise<void>;
  clearConversations(userId?: string): Promise<void>;

  // Session store for authentication
  sessionStore: any;
}
