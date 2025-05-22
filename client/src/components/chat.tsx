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
      console.error("Failed to send message:", error);
      toast({
        title: "Error",
        description: "Failed to send message. Please try again.",
        variant: "destructive",
      });
      
      // Remove loading message
      setMessages(prev => prev.filter(msg => !msg.isLoading));
      setIsProcessing(false);
    }
  };

  // Scroll to bottom when messages change
  useEffect(() => {
    if (scrollAreaRef.current) {
      const scrollArea = scrollAreaRef.current;
      scrollArea.scrollTop = scrollArea.scrollHeight;
    }
  }, [messages]);

  // TURN OFF automatic message updates from server
  // This will prevent duplicate messages - we will control this manually
  useEffect(() => {
    if (conversation && conversation.messages && messages.length === 0) {
      // Only update messages when they're empty (first load)
      console.log("Initial load of messages:", conversation.messages.length);
      setMessages(conversation.messages);
      setIsProcessing(false);
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
