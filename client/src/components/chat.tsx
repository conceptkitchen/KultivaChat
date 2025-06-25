import React, { useEffect, useRef, useState, useMemo } from "react";
import { ScrollArea } from "@/components/ui/scroll-area";
import { ChatBubble, ChatBubbleSkeleton } from "@/components/ui/chat-bubble";
import { ChatInput } from "@/components/ui/chat-input";
import { Message, Conversation } from "@/lib/utils";
import { v4 as uuidv4 } from "uuid";
import { useMutation } from "@tanstack/react-query";
import { useToast } from "@/hooks/use-toast";

interface ChatProps {
  conversation: Conversation;
}

export function Chat({ conversation }: ChatProps) {
  const scrollAreaRef = useRef<HTMLDivElement>(null);
  const [messages, setMessages] = useState<Message[]>([]);
  const [isProcessing, setIsProcessing] = useState(false);
  const { toast } = useToast();
  const initializedRef = useRef<string>("");
  
  // Memoize welcome message to prevent recreation
  const welcomeMessage = useMemo(() => ({
    id: `welcome-${conversation?.id}`,
    role: "assistant" as const,
    content: "Hello! I'm Kultivate AI, your data assistant. I can help you with:\n\n• Analyzing and visualizing your data\n• Creating code snippets for your Keboola integrations\n• Generating documentation and reports\n• Answering questions about your data pipeline\n\nWhat would you like to work on today?",
    timestamp: new Date(),
  }), [conversation?.id]);
  
  // Initialize messages only once per conversation
  useEffect(() => {
    if (!conversation?.id || initializedRef.current === conversation.id) return;
    
    initializedRef.current = conversation.id;
    
    if (conversation.messages && conversation.messages.length > 0) {
      setMessages(conversation.messages);
    } else {
      setMessages([welcomeMessage]);
    }
  }, [conversation?.id, welcomeMessage]);

  // Auto-scroll
  useEffect(() => {
    if (messages.length > 0 && scrollAreaRef.current) {
      const scrollToBottom = () => {
        if (scrollAreaRef.current) {
          scrollAreaRef.current.scrollTop = scrollAreaRef.current.scrollHeight;
        }
      };
      setTimeout(scrollToBottom, 50);
    }
  }, [messages]);

  const sendMessageMutation = useMutation({
    mutationFn: async (content: string) => {
      const response = await fetch(`/api/conversations/${conversation.id}/messages`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          message: content
        })
      });
      if (!response.ok) throw new Error(`HTTP ${response.status}`);
      return response.json();
    },
    onSuccess: (data) => {
      const displays = data.displays || [];
      
      const assistantMessage: Message = {
        id: uuidv4(),
        role: 'assistant',
        content: data.reply || "No response",
        displays: displays,
        timestamp: new Date(),
      };

      // Simply add the assistant response
      setMessages(prev => [...prev, assistantMessage]);
      setIsProcessing(false);
    },
    onError: (error) => {
      setIsProcessing(false);
      toast({
        title: "Error",
        description: "Failed to send message. Please try again.",
        variant: "destructive",
      });
    }
  });

  const handleSendMessage = (content: string) => {
    if (isProcessing || !content.trim()) return;
    
    const trimmedContent = content.trim();
    
    // Prevent duplicate messages
    const lastMessage = messages[messages.length - 1];
    if (lastMessage && lastMessage.role === "user" && lastMessage.content === trimmedContent) {
      return;
    }
    
    setIsProcessing(true);
    
    const userMessage: Message = {
      id: uuidv4(),
      role: "user",
      content: trimmedContent,
      timestamp: new Date(),
    };
    
    // Add ONLY user message - no loading message to prevent duplication
    setMessages(prev => [...prev, userMessage]);
    sendMessageMutation.mutate(trimmedContent);
  };

  return (
    <div className="flex flex-col h-full overflow-hidden bg-neutral-50">
      <ScrollArea 
        ref={scrollAreaRef}
        className="flex-1 p-4 md:px-8 space-y-6 custom-scrollbar overflow-y-auto max-h-[calc(100vh-160px)]"
        style={{ overscrollBehavior: "contain" }}
      >
        {messages.map((message) => (
          <ChatBubble 
            key={message.id} 
            message={message}
          />
        ))}
        {isProcessing && <ChatBubbleSkeleton />}
      </ScrollArea>

      <ChatInput 
        onSend={handleSendMessage}
        disabled={isProcessing}
      />
    </div>
  );
}