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
    } else {
      // Start with empty messages for new conversations
      setMessages([]);
    }
  }, [conversation]);

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
    onSuccess: (data, variables) => {
      console.log('Backend response received:', data);

      // Fix: Check for singular 'display' object from backend and convert to array
      const displays = data.display ? [data.display] : (data.displays || []);

      // Only create assistant message - user message was already added locally
      const assistantMessage = {
        id: `assistant-${Date.now()}`,
        role: 'assistant' as const,
        content: data.reply || data.final_answer || "No response",
        displays: displays,
        timestamp: new Date(),
      };

      console.log('Creating assistant message with displays:', assistantMessage.displays.length);

      // Remove loading message and add only the assistant response
      setMessages(prev => 
        prev.filter(msg => !msg.isLoading).concat([assistantMessage])
      );

      setIsProcessing(false);
    },
    onError: (error) => {
      console.error("Error sending message:", error);
      console.log("Error occurred:", error);
      setMessages((prev) => prev.filter((msg) => !msg.isLoading));
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
    
    // Prevent duplicate messages
    if (messages.some(msg => msg.role === "user" && msg.content === content && !msg.isLoading)) {
      return;
    }
    
    setIsProcessing(true);
    
    // Create user message
    const userMessage: Message = {
      id: uuidv4(),
      role: "user",
      content,
      timestamp: new Date(),
    };
    
    // Add loading message for AI response
    const loadingMessage: Message = {
      id: uuidv4(),
      role: "assistant",
      content: "",
      timestamp: new Date(),
      isLoading: true,
    };
    
    // Add both messages immediately
    setMessages(prev => [...prev, userMessage, loadingMessage]);
    
    console.log("Sending message to API:", content);
    
    // Send to backend
    sendMessageMutation.mutate(content);
  };

  // Auto-scroll to the bottom whenever messages change or a message is sent
  useEffect(() => {
    // Only auto-scroll when we have messages or when processing a message
    if (messages.length > 0 || isProcessing) {
      const scrollToBottom = () => {
        if (scrollAreaRef.current) {
          scrollAreaRef.current.scrollTop = scrollAreaRef.current.scrollHeight;
        }
      };
      
      // Scroll immediately
      scrollToBottom();
      
      // And again after a tiny delay to ensure DOM has updated
      setTimeout(scrollToBottom, 100);
      
      // One more time for good measure to handle slow DOM updates
      setTimeout(scrollToBottom, 300);
    }
  }, [messages, isProcessing]);

  // Each conversation is completely independent
  useEffect(() => {
    if (conversation && conversation.id) {
      console.log("Loading messages for conversation:", conversation.id);
      
      // If conversation has existing messages, show them
      if (conversation.messages && conversation.messages.length > 0) {
        setMessages(conversation.messages);
      } else {
        // Only show welcome message for truly new conversations
        const welcomeMessage = {
          id: "welcome-" + uuidv4(),
          role: "assistant" as const,
          content: "Hello! I'm Kultivate AI, your data assistant. I can help you with:\n\n• Analyzing and visualizing your data\n• Creating code snippets for your Keboola integrations\n• Generating documentation and reports\n• Answering questions about your data pipeline\n\nWhat would you like to work on today?",
          timestamp: new Date(),
        };
        setMessages([welcomeMessage]);
      }
      
      setIsProcessing(false);
    }
  }, [conversation?.id]);



  return (
    <div className="flex flex-col h-full overflow-hidden bg-neutral-50">
      <ScrollArea 
        ref={scrollAreaRef}
        className="flex-1 p-4 md:px-8 space-y-6 custom-scrollbar overflow-y-auto max-h-[calc(100vh-160px)]"
        style={{ overscrollBehavior: "contain" }}
      >
        {/* Show all messages in the conversation */}
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
