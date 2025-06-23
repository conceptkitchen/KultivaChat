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
            ? "chat-bubble-user mr-4"
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
            <div className="whitespace-pre-wrap">
              {/* Clean up JSON formatting if present */}
              {message.content.startsWith('{"content":') ? 
                JSON.parse(message.content).content : 
                message.content
              }
            </div>
            
            {!isUser && message.displays && message.displays.length > 0 && (
              <div className="mt-4">
                <div className="mb-2 p-2 bg-blue-50 border border-blue-200 rounded text-xs">
                  DEBUG: Found {message.displays.length} displays. First display: {message.displays[0]?.type} with {Array.isArray(message.displays[0]?.content) ? message.displays[0].content.length : 0} items
                </div>
                <CanvasDisplay displays={message.displays} />
                {message.displays[0]?.type === "table" && (
                  <div className="mt-2 border rounded-md bg-white p-4">
                    <h3 className="font-semibold mb-3 text-gray-800">Backup Table Display</h3>
                    <div className="overflow-auto max-h-96">
                      <table className="w-full border-collapse border border-gray-300">
                        <thead>
                          <tr className="bg-gray-50">
                            <th className="border border-gray-300 px-3 py-2 text-left font-semibold text-xs">Table Name</th>
                          </tr>
                        </thead>
                        <tbody>
                          {(message.displays[0].content as any[]).slice(0, 10).map((row: any, idx: number) => (
                            <tr key={idx} className={idx % 2 === 0 ? "bg-white" : "bg-gray-50"}>
                              <td className="border border-gray-300 px-3 py-2 text-sm">{row.table_name}</td>
                            </tr>
                          ))}
                        </tbody>
                      </table>
                    </div>
                    <div className="mt-2 text-xs text-gray-600">
                      Showing first 10 of {(message.displays[0].content as any[]).length} tables
                    </div>
                  </div>
                )}
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
