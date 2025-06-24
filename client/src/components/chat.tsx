import React, { useEffect, useRef, useState, useCallback } from "react";
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
  const lastConversationId = useRef<string>("");
  const messageIdSet = useRef<Set<string>>(new Set());
  
  // Initialize messages when conversation changes
  useEffect(() => {
    if (!conversation?.id) return;
    
    // Only update if conversation actually changed
    if (lastConversationId.current !== conversation.id) {
      lastConversationId.current = conversation.id;
      messageIdSet.current.clear();
      
      if (conversation.messages && conversation.messages.length > 0) {
        const uniqueMessages = conversation.messages.filter(msg => {
          if (messageIdSet.current.has(msg.id)) return false;
          messageIdSet.current.add(msg.id);
          return true;
        });
        setMessages(uniqueMessages);
      } else {
        const welcomeId = "welcome-" + conversation.id;
        if (!messageIdSet.current.has(welcomeId)) {
          const welcomeMessage: Message = {
            id: welcomeId,
            role: "assistant",
            content: "Hello! I'm Kultivate AI, your data assistant. I can help you with:\n\n• Analyzing and visualizing your data\n• Creating code snippets for your Keboola integrations\n• Generating documentation and reports\n• Answering questions about your data pipeline\n\nWhat would you like to work on today?",
            timestamp: new Date(),
          };
          messageIdSet.current.add(welcomeId);
          setMessages([welcomeMessage]);
        }
      }
    }
  }, [conversation?.id]);

  // Auto-scroll
  useEffect(() => {
    if (messages.length > 0 && scrollAreaRef.current) {
      setTimeout(() => {
        if (scrollAreaRef.current) {
          scrollAreaRef.current.scrollTop = scrollAreaRef.current.scrollHeight;
        }
      }, 50);
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
      
      const assistantMessage: Message = {
        id: uuidv4(),
        role: 'assistant',
        content: data.reply || data.final_answer || "No response",
        displays: displays,
        timestamp: new Date(),
      };

      messageIdSet.current.add(assistantMessage.id);

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

  const handleSendMessage = useCallback(async (content: string) => {
    if (isProcessing || !content.trim()) return;
    
    const trimmedContent = content.trim();
    
    // Check for exact duplicate in recent messages
    const isDuplicate = messages.slice(-5).some(msg => 
      msg.role === "user" && msg.content === trimmedContent
    );
    
    if (isDuplicate) {
      console.log("Duplicate message prevented:", trimmedContent);
      return;
    }
    
    setIsProcessing(true);
    
    const userMessageId = uuidv4();
    const loadingMessageId = uuidv4();
    
    const userMessage: Message = {
      id: userMessageId,
      role: "user",
      content: trimmedContent,
      timestamp: new Date(),
    };
    
    const loadingMessage: Message = {
      id: loadingMessageId,
      role: "assistant",
      content: "",
      timestamp: new Date(),
      isLoading: true,
    };
    
    // Track message IDs
    messageIdSet.current.add(userMessageId);
    messageIdSet.current.add(loadingMessageId);
    
    setMessages(prev => [...prev, userMessage, loadingMessage]);
    sendMessageMutation.mutate(trimmedContent);
  }, [isProcessing, messages, sendMessageMutation]);

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