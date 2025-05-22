import React, { useState, useRef, useEffect } from "react";
import { Paperclip, Send } from "lucide-react";
import { Button } from "@/components/ui/button";
import { Textarea } from "@/components/ui/textarea";

interface ChatInputProps {
  onSend: (message: string) => void;
  conversationId?: string; // Add conversationId prop
  disabled?: boolean;
  placeholder?: string;
}

export function ChatInput({ onSend, disabled = false, placeholder = "Ask a question..." }: ChatInputProps) {
  const [message, setMessage] = useState("");
  const textareaRef = useRef<HTMLTextAreaElement>(null);

  const handleSubmit = (e: React.FormEvent) => {
    e.preventDefault();
    if (message.trim() && !disabled) {
      onSend(message);
      setMessage("");
      
      // Reset textarea height
      if (textareaRef.current) {
        textareaRef.current.style.height = "auto";
      }
    }
  };

  // Auto-resize textarea
  useEffect(() => {
    const textarea = textareaRef.current;
    if (!textarea) return;

    const resizeTextarea = () => {
      textarea.style.height = "auto";
      textarea.style.height = `${textarea.scrollHeight}px`;
    };

    textarea.addEventListener("input", resizeTextarea);
    return () => {
      textarea.removeEventListener("input", resizeTextarea);
    };
  }, []);

  return (
    <div className="border-t border-neutral-200 bg-white p-4">
      <div className="max-w-4xl mx-auto">
        <form className="relative" onSubmit={handleSubmit}>
          <Textarea
            ref={textareaRef}
            className="w-full border border-neutral-300 rounded-lg px-4 py-3 pr-20 focus:outline-none focus:ring-2 focus:ring-primary focus:border-primary resize-none"
            placeholder={placeholder}
            rows={2}
            value={message}
            onChange={(e) => setMessage(e.target.value)}
            disabled={disabled}
            onKeyDown={(e) => {
              if (e.key === "Enter" && !e.shiftKey) {
                e.preventDefault();
                handleSubmit(e);
              }
            }}
          />
          <div className="absolute right-3 bottom-3 flex items-center">
            <Button
              type="button"
              variant="ghost"
              size="icon"
              className="text-neutral-400 hover:text-neutral-600"
            >
              <Paperclip className="h-5 w-5" />
            </Button>
            <Button
              type="submit"
              size="icon"
              className="ml-2 bg-primary hover:bg-primary-dark text-white rounded-md p-2 transition-colors"
              disabled={!message.trim() || disabled}
            >
              <Send className="h-5 w-5" />
            </Button>
          </div>
        </form>
        <div className="text-xs text-neutral-500 mt-2">
          Kultivate AI can make mistakes. Check data sources when using for critical tasks.
        </div>
      </div>
    </div>
  );
}
