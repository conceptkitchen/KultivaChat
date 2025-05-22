import React, { useEffect } from "react";
import { useRoute, useLocation } from "wouter";
import { Chat } from "@/components/chat";
import { useQuery, useMutation } from "@tanstack/react-query";
import { Conversation } from "@/lib/utils";
import { Skeleton } from "@/components/ui/skeleton";
import { apiRequest } from "@/lib/queryClient";
import { queryClient } from "@/lib/queryClient";
import { v4 as uuidv4 } from "uuid";

export default function ChatPage() {
  const [match, params] = useRoute<{ id: string }>("/chat/:id");
  const [, navigate] = useLocation();
  const conversationId = params?.id;
  
  // Create a new conversation if we're on the root path
  const createConversationMutation = useMutation({
    mutationFn: async () => {
      const response = await apiRequest("POST", "/api/conversations", {
        title: "New Conversation",
      });
      return response.json();
    },
    onSuccess: (data) => {
      queryClient.invalidateQueries({ queryKey: ["/api/conversations"] });
      navigate(`/chat/${data.id}`);
    },
  });
  
  // If we're at the root path with no conversation ID, create one
  useEffect(() => {
    if (!conversationId && !match) {
      createConversationMutation.mutate();
    }
  }, [conversationId, match]);

  const { data: conversation, isLoading, error } = useQuery<Conversation>({
    queryKey: ["/api/conversations", conversationId],
    enabled: !!conversationId,
  });

  if (createConversationMutation.isPending || isLoading) {
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

  if ((error || !conversation) && conversationId) {
    return (
      <div className="h-full flex items-center justify-center">
        <div className="text-center p-8">
          <h2 className="text-2xl font-bold text-neutral-700 mb-2">Conversation not found</h2>
          <p className="text-neutral-500">This conversation does not exist or has been deleted.</p>
        </div>
      </div>
    );
  }

  if (!conversation) {
    return null; // This should not happen as we redirect to a new conversation
  }

  console.log("Chat page rendering conversation:", conversation);
  return <Chat conversation={conversation} />;
}
