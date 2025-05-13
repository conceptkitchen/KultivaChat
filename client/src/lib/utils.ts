import { type ClassValue, clsx } from "clsx";
import { twMerge } from "tailwind-merge";

export function cn(...inputs: ClassValue[]) {
  return twMerge(clsx(inputs));
}

export type ContentType = "text" | "code" | "documentation" | "table" | "visualization";

export interface DisplayContent {
  type: ContentType;
  title?: string;
  language?: string;
  content: string | Record<string, any>[] | { [key: string]: any };
}

export interface Message {
  id: string;
  role: "user" | "assistant";
  content: string;
  timestamp: Date;
  displays?: DisplayContent[];
  isLoading?: boolean;
}

export interface Conversation {
  id: string;
  title: string;
  messages: Message[];
  createdAt: Date;
  updatedAt: Date;
}

export function formatDate(date: Date): string {
  return new Intl.DateTimeFormat("en-US", {
    month: "short",
    day: "numeric",
    hour: "numeric", 
    minute: "2-digit", 
    hour12: true
  }).format(date);
}
