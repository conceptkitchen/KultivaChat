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
    mutationFn: async (message: { conversationId: string; content: string }) => {
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
      toast({
        title: "Error",
        description: "Failed to send message. Please try again.",
        variant: "destructive",
      });
      // Remove the loading messages
      setMessages((prev) => prev.filter((msg) => !msg.isLoading));
      setIsProcessing(false);
    }
  });

  const handleSendMessage = async (content: string) => {
    if (isProcessing) return;
    
    setIsProcessing(true);
    
    // Only show the message locally until the server responds
    // This prevents duplicate messages in the UI
    const tempMessages = [...messages];
    
    // Create new message objects
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
    
    // Add temporary messages for UI display only
    tempMessages.push(userMessage, loadingMessage);
    setMessages(tempMessages);
    
    console.log("Sending message to API:", content);
    
    // Send to API
    sendMessageMutation.mutate({
      conversationId: conversation.id,
      content,
    });
  };

  // Scroll to bottom when messages change
  useEffect(() => {
    if (scrollAreaRef.current) {
      const scrollArea = scrollAreaRef.current;
      scrollArea.scrollTop = scrollArea.scrollHeight;
    }
  }, [messages]);

  // Update local messages when conversation updates
  useEffect(() => {
    if (conversation && conversation.messages) {
      // Only update messages from the server if they're different
      // This prevents duplicate messages from appearing
      if (JSON.stringify(messages) !== JSON.stringify(conversation.messages)) {
        console.log("Updating messages from server - count:", conversation.messages.length);
        setMessages(conversation.messages);
        setIsProcessing(false); // Clear processing state when we get a response
      }
    }
  }, [conversation]);

  // Initial welcome message if this is a new conversation
  useEffect(() => {
    if (messages.length === 0) {
      setMessages([
        {
          id: uuidv4(),
          role: "assistant",
          content: "Hello! I'm Kultivate AI, your data assistant. I can help you with:\n\n• Analyzing and visualizing your data\n• Creating code snippets for your Keboola integrations\n• Generating documentation and reports\n• Answering questions about your data pipeline\n\nWhat would you like to work on today?",
          timestamp: new Date(),
        },
      ]);
    }
  }, [messages]);

  return (
    <div className="flex flex-col h-full overflow-hidden bg-neutral-50">
      <ScrollArea 
        ref={scrollAreaRef}
        className="flex-1 p-4 md:px-8 space-y-6 custom-scrollbar"
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
