import React from "react";
import { useRoute } from "wouter";
import { Chat } from "@/components/chat";
import { useQuery } from "@tanstack/react-query";
import { Conversation } from "@/lib/utils";
import { Skeleton } from "@/components/ui/skeleton";

export default function ChatPage() {
  const [match, params] = useRoute<{ id: string }>("/chat/:id");
  const conversationId = params?.id;

  const { data: conversation, isLoading, error } = useQuery<Conversation>({
    queryKey: ["/api/conversations", conversationId],
    enabled: !!conversationId,
  });

  if (isLoading) {
    return (
      <div className="h-full flex flex-col">
        <div className="flex-1 p-8 space-y-6">
          <Skeleton className="h-10 w-10 rounded-full" />
          <Skeleton className="h-24 w-full max-w-3xl rounded-lg" />
          <div className="flex justify-end">
            <Skeleton className="h-24 w-full max-w-3xl rounded-lg" />
          </div>
        </div>
        <div className="h-24 border-t border-neutral-200" />
      </div>
    );
  }

  if (error || !conversation) {
    return (
      <div className="h-full flex items-center justify-center">
        <div className="text-center p-8">
          <h2 className="text-2xl font-bold text-neutral-700 mb-2">Conversation not found</h2>
          <p className="text-neutral-500">This conversation does not exist or has been deleted.</p>
        </div>
      </div>
    );
  }

  return <Chat conversation={conversation} />;
}
