import React from "react";
import { cn } from "@/lib/utils";
import { Message } from "@/lib/utils";
import { CanvasDisplay } from "@/components/ui/canvas-display";
import { AssistantAI } from "@/components/ui/assistant-ai";
import { Skeleton } from "@/components/ui/skeleton";

interface ChatBubbleProps {
  message: Message;
  className?: string;
}

export function ChatBubble({ message, className }: ChatBubbleProps) {
  const isUser = message.role === "user";

  return (
    <div
      className={cn(
        "flex items-start max-w-4xl mx-auto",
        isUser ? "justify-end mb-6" : "mb-6",
        className
      )}
    >
      {!isUser && (
        <AssistantAI className="flex-shrink-0" />
      )}

      <div
        className={cn(
          "shadow-sm p-4 rounded-2xl max-w-3xl",
          isUser
            ? "bg-blue-600 text-white chat-bubble-user mr-4"
            : "bg-white text-neutral-800 chat-bubble-ai ml-4"
        )}
      >
        {message.isLoading ? (
          <div className="flex items-center space-x-2">
            <div className="h-4 w-4 bg-primary rounded-full animate-spin" />
            <span className={isUser ? "text-white" : "text-neutral-500"}>
              {isUser ? "Sending..." : "Generating response..."}
            </span>
          </div>
        ) : (
          <div 
            className={cn(
              "leading-relaxed",
              isUser ? "text-white" : "text-neutral-800" 
            )}
          >
            <div className="whitespace-pre-wrap">{message.content}</div>
            
            {!isUser && message.displays && message.displays.length > 0 && (
              <div className="mt-4">
                <CanvasDisplay displays={message.displays} />
              </div>
            )}
          </div>
        )}
      </div>

      {isUser && (
        <div className="flex-shrink-0 h-10 w-10 rounded-full bg-neutral-200 flex items-center justify-center">
          <svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2" strokeLinecap="round" strokeLinejoin="round" className="text-neutral-500"><path d="M19 21v-2a4 4 0 0 0-4-4H9a4 4 0 0 0-4 4v2"></path><circle cx="12" cy="7" r="4"></circle></svg>
        </div>
      )}
    </div>
  );
}

export function ChatBubbleSkeleton() {
  return (
    <div className="flex items-start max-w-4xl mx-auto mb-6">
      <Skeleton className="h-10 w-10 rounded-full" />
      <div className="ml-4 bg-white shadow-sm p-4 rounded-2xl max-w-3xl w-full">
        <Skeleton className="h-4 w-3/4 mb-2" />
        <Skeleton className="h-4 w-full mb-2" />
        <Skeleton className="h-4 w-1/2" />
      </div>
    </div>
  );
}
