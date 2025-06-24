import React, { useEffect, useRef, useState } from "react";
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
  
  // Initialize messages only once when conversation changes
  useEffect(() => {
    if (conversation?.id) {
      if (conversation.messages && conversation.messages.length > 0) {
        setMessages(conversation.messages);
      } else {
        // Welcome message for new conversations
        const welcomeMessage = {
          id: "welcome-" + uuidv4(),
          role: "assistant" as const,
          content: "Hello! I'm Kultivate AI, your data assistant. I can help you with:\n\n• Analyzing and visualizing your data\n• Creating code snippets for your Keboola integrations\n• Generating documentation and reports\n• Answering questions about your data pipeline\n\nWhat would you like to work on today?",
          timestamp: new Date(),
        };
        setMessages([welcomeMessage]);
      }
    }
  }, [conversation?.id]);

  // Auto-scroll when messages change
  useEffect(() => {
    if (messages.length > 0 && scrollAreaRef.current) {
      const scrollToBottom = () => {
        if (scrollAreaRef.current) {
          scrollAreaRef.current.scrollTop = scrollAreaRef.current.scrollHeight;
        }
      };
      scrollToBottom();
      setTimeout(scrollToBottom, 100);
    }
  }, [messages]);

  const sendMessageMutation = useMutation({
    mutationFn: async (content: string) => {
      const response = await fetch('/api/chat', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          message: content,
          conversation_history: []
        })
      });
      if (!response.ok) throw new Error(`HTTP ${response.status}`);
      return response.json();
    },
    onSuccess: (data) => {
      const displays = data.display ? [data.display] : (data.displays || []);
      
      const assistantMessage = {
        id: `assistant-${Date.now()}`,
        role: 'assistant' as const,
        content: data.reply || data.final_answer || "No response",
        displays: displays,
        timestamp: new Date(),
      };

      // Replace loading message with assistant response
      setMessages(prev => 
        prev.filter(msg => !msg.isLoading).concat([assistantMessage])
      );
      setIsProcessing(false);
    },
    onError: (error) => {
      console.error("Chat error:", error);
      setMessages(prev => prev.filter(msg => !msg.isLoading));
      setIsProcessing(false);
      toast({
        title: "Error",
        description: "Failed to send message. Please try again.",
        variant: "destructive",
      });
    }
  });

  const handleSendMessage = async (content: string) => {
    if (isProcessing) return;
    
    // Check for recent duplicate
    const recentUserMessages = messages.filter(msg => 
      msg.role === "user" && 
      msg.content === content && 
      Date.now() - msg.timestamp.getTime() < 5000 // Within 5 seconds
    );
    
    if (recentUserMessages.length > 0) {
      return; // Skip duplicate
    }
    
    setIsProcessing(true);
    
    const userMessage: Message = {
      id: uuidv4(),
      role: "user",
      content,
      timestamp: new Date(),
    };
    
    const loadingMessage: Message = {
      id: uuidv4(),
      role: "assistant",
      content: "",
      timestamp: new Date(),
      isLoading: true,
    };
    
    // Add messages
    setMessages(prev => [...prev, userMessage, loadingMessage]);
    
    // Send to backend
    sendMessageMutation.mutate(content);
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
        {sendMessageMutation.isPending && <ChatBubbleSkeleton />}
      </ScrollArea>

      <ChatInput 
        onSend={handleSendMessage}
        disabled={isProcessing}
      />
    </div>
  );
}