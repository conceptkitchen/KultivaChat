import React, { useEffect, useRef, useState } from "react";
import { ScrollArea } from "@/components/ui/scroll-area";
import { ChatBubble, ChatBubbleSkeleton } from "@/components/ui/chat-bubble";
import { ChatInput } from "@/components/ui/chat-input";
import { Message, Conversation } from "@/lib/utils";
import { v4 as uuidv4 } from "uuid";
import { apiRequest } from "@/lib/queryClient";
import { useMutation, useQueryClient } from "@tanstack/react-query";
import { useToast } from "@/hooks/use-toast";

interface ChatProps {
  conversation: Conversation;
}

export function Chat({ conversation }: ChatProps) {
  const scrollAreaRef = useRef<HTMLDivElement>(null);
  const [messages, setMessages] = useState<Message[]>([]);
  const [isProcessing, setIsProcessing] = useState(false);
  const { toast } = useToast();
  const queryClient = useQueryClient();
  
  // For debugging
  console.log("Chat rendering with conversation:", conversation.id);
  
  // Update messages when conversation changes
  useEffect(() => {
    if (conversation && conversation.messages) {
      setMessages(conversation.messages);
      console.log("Updated messages from conversation:", conversation.messages.length);
    }
  }, [conversation]);

  const sendMessageMutation = useMutation({
    mutationFn: async (message: { conversationId: string; content: string; systemMessage?: string }) => {
      return await apiRequest("POST", "/api/messages", message);
    },
    onSuccess: (response) => {
      // Update the conversation in the cache
      queryClient.invalidateQueries({ queryKey: ["/api/conversations"] });
      queryClient.invalidateQueries({ 
        queryKey: ["/api/conversations", conversation.id] 
      });
    },
    onError: (error) => {
      // Only show real errors with message text
      if (error instanceof Error && error.message && 
          error.message !== '{}' && 
          error.message !== 'Failed to fetch') {
        console.error("Actual error sending message:", error);
        toast({
          title: "Error",
          description: "Failed to send message. Please try again.",
          variant: "destructive",
        });
      }
      // Remove the loading messages
      setMessages((prev) => prev.filter((msg) => !msg.isLoading));
      setIsProcessing(false);
    }
  });

  const handleSendMessage = async (content: string) => {
    if (isProcessing) return;
    
    // STRICT duplicate check
    if (messages.some(msg => msg.role === "user" && msg.content === content)) {
      toast({
        title: "Duplicate message",
        description: "You've already sent this message",
      });
      return;
    }
    
    setIsProcessing(true);
    
    // Create new message objects with fixed IDs
    const userMessage: Message = {
      id: uuidv4(),
      role: "user",
      content,
      timestamp: new Date(),
    };
    
    // Add AI "loading" message
    const loadingMessage: Message = {
      id: uuidv4(),
      role: "assistant",
      content: "",
      timestamp: new Date(),
      isLoading: true,
    };
    
    // Add to local messages only - don't use server data
    const updatedMessages = [...messages, userMessage, loadingMessage];
    setMessages(updatedMessages);
    
    console.log("Sending message to API:", content);
    
    try {
      // Send to API
      const response = await sendMessageMutation.mutateAsync({
        conversationId: conversation.id,
        content,
      });
      
      // Get response data
      const data = await response.json();
      
      // Manually update messages with the response
      const aiMessage: Message = {
        id: data.aiMessage.id,
        role: "assistant",
        content: data.aiMessage.content,
        displays: data.aiMessage.displays || [],
        timestamp: new Date(data.aiMessage.timestamp),
      };
      
      // Replace loading message with actual response
      setMessages(prev => 
        prev.map(msg => msg.isLoading ? aiMessage : msg)
      );
      
      setIsProcessing(false);
    } catch (error) {
      // Only log the error but don't show toast
      console.error("Error occurred:", error);
      
      // Remove loading message
      setMessages(prev => prev.filter(msg => !msg.isLoading));
      setIsProcessing(false);
    }
  };

  // Make sure all messages are visible, especially the welcome message
  useEffect(() => {
    if (scrollAreaRef.current) {
      const scrollArea = scrollAreaRef.current;
      
      // Always scroll to bottom to show latest messages
      scrollArea.scrollTop = scrollArea.scrollHeight;
    }
  }, [messages.length]);

  // Each conversation is completely independent
  useEffect(() => {
    if (conversation && conversation.id) {
      // When conversation ID changes, reset the message state
      console.log("Loading messages for conversation:", conversation.id);
      
      // Create a welcome message with a permanent ID that stays at the beginning
      const welcomeMessage = {
        id: "welcome-message-" + conversation.id,
        role: "assistant" as const,
        content: "Hello! I'm Kultivate AI, your data assistant. I can help you with:\n\n• Analyzing and visualizing your data\n• Creating code snippets for your Keboola integrations\n• Generating documentation and reports\n• Answering questions about your data pipeline\n\nWhat would you like to work on today?",
        timestamp: new Date(conversation.createdAt),
      };
      
      // Always include welcome message before other messages (at the top)
      const allMessages = [welcomeMessage, ...(conversation.messages || [])];
      
      // Set messages with welcome message
      setMessages(allMessages);
      setIsProcessing(false);
    }
  }, [conversation?.id]);

  // Add welcome message to the conversation
  useEffect(() => {
    // Only show welcome message for conversations that have no messages or have user messages
    if (conversation && messages.length === 0) {
      const welcomeMessage = {
        id: "welcome-" + uuidv4(),
        role: "assistant" as const,
        content: "Hello! I'm Kultivate AI, your data assistant. I can help you with:\n\n• Analyzing and visualizing your data\n• Creating code snippets for your Keboola integrations\n• Generating documentation and reports\n• Answering questions about your data pipeline\n\nWhat would you like to work on today?",
        timestamp: new Date(),
      };
      
      // Add welcome message at beginning
      setMessages([welcomeMessage]);
    }
  }, [conversation, messages.length]);

  return (
    <div className="flex flex-col h-full overflow-hidden bg-neutral-50">
      <ScrollArea 
        ref={scrollAreaRef}
        className="flex-1 p-4 md:px-8 space-y-6 custom-scrollbar overflow-y-auto max-h-[calc(100vh-160px)]"
      >
        {messages.map((message) => (
          <ChatBubble 
            key={message.id} 
            message={message}
          />
        ))}
        {sendMessageMutation.isPending && <ChatBubbleSkeleton />}
      </ScrollArea>

      <ChatInput 
        onSend={handleSendMessage}
        disabled={isProcessing}
      />
    </div>
  );
}
