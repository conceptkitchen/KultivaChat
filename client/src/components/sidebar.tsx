import React, { useEffect, useState } from "react";
import { Button } from "@/components/ui/button";
import { ScrollArea } from "@/components/ui/scroll-area";
import { 
  MessageSquare, 
  Plus, 
  Trash2, 
  LogOut,
  Menu
} from "lucide-react";
import { cn } from "@/lib/utils";
import { useLocation, Link } from "wouter";
import { Conversation } from "@/lib/utils";
import { apiRequest } from "@/lib/queryClient";
import { useQuery, useMutation } from "@tanstack/react-query";
import { queryClient } from "@/lib/queryClient";

interface SidebarProps {
  isOpen: boolean;
  onClose: () => void;
  onNewChat: () => void;
}

export function Sidebar({ isOpen, onClose, onNewChat }: SidebarProps) {
  const [location, navigate] = useLocation();
  const { data: conversations = [] } = useQuery<Conversation[]>({
    queryKey: ['/api/conversations'],
  });

  const clearConversationsMutation = useMutation({
    mutationFn: async () => {
      return await apiRequest('DELETE', '/api/conversations', {});
    },
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['/api/conversations'] });
      // Always create a fresh conversation
      navigate('/');
    }
  });

  // Close sidebar when clicking outside on mobile
  useEffect(() => {
    const handleOutsideClick = (e: MouseEvent) => {
      if (isOpen && window.innerWidth < 768) {
        const sidebar = document.getElementById('sidebar');
        const toggle = document.getElementById('sidebar-toggle');
        if (
          sidebar && 
          !sidebar.contains(e.target as Node) && 
          toggle && 
          !toggle.contains(e.target as Node)
        ) {
          onClose();
        }
      }
    };

    document.addEventListener('mousedown', handleOutsideClick);
    return () => {
      document.removeEventListener('mousedown', handleOutsideClick);
    };
  }, [isOpen, onClose]);

  return (
    <>
      {/* Mobile sidebar overlay */}
      {isOpen && (
        <div 
          className="fixed inset-0 bg-black bg-opacity-50 z-40 md:hidden" 
          onClick={onClose}
        />
      )}

      <aside
        id="sidebar"
        className={cn(
          "w-64 bg-white border-r border-neutral-100 z-50 transition-all duration-300 flex-shrink-0",
          "fixed md:static inset-y-0 left-0 transform md:transform-none",
          isOpen ? "translate-x-0" : "-translate-x-full md:translate-x-0",
          "md:flex flex-col h-full",
          isOpen ? "flex" : "hidden"
        )}
      >
        <div className="flex flex-col h-full">
          <div className="p-4">
            <Button 
              className="w-full bg-primary hover:bg-primary-dark text-white transition-colors"
              onClick={() => {
                onNewChat();
                if (window.innerWidth < 768) {
                  onClose();
                }
              }}
            >
              <Plus className="mr-2 h-4 w-4" />
              <span>New Chat</span>
            </Button>
          </div>
          
          <div className="px-3 py-2 text-sm font-medium text-neutral-400">
            Recent conversations
          </div>
          
          <ScrollArea className="flex-1">
            <div className="px-2 space-y-1">
              {conversations.map((conversation) => (
                <Button
                  key={conversation.id}
                  variant="ghost"
                  className={cn(
                    "w-full justify-start px-3 py-2",
                    location === `/chat/${conversation.id}` 
                      ? "bg-neutral-100 text-primary"
                      : "text-neutral-700 hover:bg-neutral-100"
                  )}
                  onClick={() => {
                    navigate(`/chat/${conversation.id}`);
                    if (window.innerWidth < 768) {
                      onClose();
                    }
                  }}
                >
                  <MessageSquare className={cn(
                    "mr-2 h-4 w-4",
                    location === `/chat/${conversation.id}` ? "text-primary" : "text-neutral-400"
                  )} />
                  <span className="truncate">{conversation.title}</span>
                </Button>
              ))}

              {conversations.length === 0 && (
                <div className="px-3 py-2 text-sm text-neutral-500">
                  No conversations yet
                </div>
              )}
            </div>
          </ScrollArea>
          
          <div className="border-t border-neutral-100 p-3">
            <Button 
              variant="ghost" 
              className="w-full justify-start px-3 py-2 text-neutral-600"
              onClick={() => clearConversationsMutation.mutate()}
              disabled={clearConversationsMutation.isPending}
            >
              <Trash2 className="text-neutral-400 mr-2 h-4 w-4" />
              <span>Clear conversations</span>
            </Button>
            <Button 
              variant="ghost" 
              className="w-full justify-start px-3 py-2 text-neutral-600"
              onClick={() => {
                window.location.href = "/api/logout";
              }}
            >
              <LogOut className="text-neutral-400 mr-2 h-4 w-4" />
              <span>Log out</span>
            </Button>
          </div>
        </div>
      </aside>
    </>
  );
}

export function SidebarButton({ onClick }: { onClick: () => void }) {
  return (
    <Button 
      id="sidebar-toggle" 
      variant="ghost" 
      size="icon" 
      className="md:hidden text-neutral-500"
      onClick={onClick}
    >
      <Menu className="h-5 w-5" />
    </Button>
  );
}
