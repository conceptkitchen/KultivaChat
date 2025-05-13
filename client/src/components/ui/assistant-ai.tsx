import React from "react";
import { cn } from "@/lib/utils";

interface AssistantAIProps {
  className?: string;
  size?: "sm" | "md" | "lg";
}

export function AssistantAI({ className, size = "md" }: AssistantAIProps) {
  const sizeClasses = {
    sm: "h-8 w-8",
    md: "h-10 w-10",
    lg: "h-12 w-12"
  };

  return (
    <div
      className={cn(
        "flex-shrink-0 rounded-full bg-primary flex items-center justify-center text-white",
        sizeClasses[size],
        className
      )}
    >
      <svg 
        xmlns="http://www.w3.org/2000/svg" 
        width="16" 
        height="16" 
        viewBox="0 0 24 24" 
        fill="none" 
        stroke="currentColor" 
        strokeWidth="2.5" 
        strokeLinecap="round" 
        strokeLinejoin="round" 
        className="lucide lucide-bot"
      >
        <path d="M12 8V4H8"></path>
        <rect width="16" height="12" x="4" y="8" rx="2"></rect>
        <path d="M2 14h2"></path>
        <path d="M20 14h2"></path>
        <path d="M15 13v2"></path>
        <path d="M9 13v2"></path>
      </svg>
    </div>
  );
}
