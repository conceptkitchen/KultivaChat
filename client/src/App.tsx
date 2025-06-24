import { useState } from "react";
import { Switch, Route, useLocation } from "wouter";
import { queryClient } from "./lib/queryClient";
import { Toaster } from "@/components/ui/toaster";
import { TooltipProvider } from "@/components/ui/tooltip";
import NotFound from "@/pages/not-found";
import Home from "@/pages/home";
import ChatPage from "@/pages/chat";
import LandingPage from "@/pages/landing";
import { Sidebar, SidebarButton } from "@/components/sidebar";
import { useMutation } from "@tanstack/react-query";
import { apiRequest } from "@/lib/queryClient";
import { AuthProvider, useAuth } from "@/hooks/use-auth";
import { ProtectedRoute } from "@/lib/protected-route";
import AuthPage from "@/pages/auth-page";
import { LogOut } from "lucide-react";
import { Button } from "@/components/ui/button";

function HeaderLogoutButton() {
  const { logoutMutation } = useAuth();
  
  return (
    <Button 
      variant="ghost" 
      size="sm"
      onClick={(e) => {
        e.preventDefault();
        e.stopPropagation();
        logoutMutation.mutate();
      }}
      disabled={logoutMutation.isPending}
      className="text-neutral-600 hover:text-neutral-800"
      type="button"
    >
      <LogOut className="w-4 h-4" />
    </Button>
  );
}

function AppRouter() {
  return (
    <Switch>
      <Route path="/" component={LandingPage} />
      <Route path="/auth" component={AuthPage} />
      <ProtectedRoute path="/settings" component={Home} />
      <ProtectedRoute path="/dashboard" component={ChatPage} />
      <ProtectedRoute path="/chat/:id" component={ChatPage} />
      <Route component={NotFound} />
    </Switch>
  );
}

function AuthContent() {
  const { user, isLoading } = useAuth();
  const [sidebarOpen, setSidebarOpen] = useState(false);
  const [location, navigate] = useLocation();

  const createConversationMutation = useMutation({
    mutationFn: async () => {
      const response = await apiRequest("POST", "/api/conversations", {
        title: "New Conversation",
      });
      return response.json();
    },
    onSuccess: (data) => {
      queryClient.invalidateQueries({ queryKey: ["/api/conversations"] });
      navigate(`/chat/${data.id}`);
    },
  });

  const handleNewChat = () => {
    createConversationMutation.mutate();
  };

  // Show loading state while checking authentication
  if (isLoading) {
    return (
      <div className="flex items-center justify-center h-screen bg-neutral-50">
        <div className="text-center">
          <div className="animate-spin rounded-full h-12 w-12 border-b-2 border-primary mx-auto"></div>
          <p className="mt-4 text-neutral-600">Loading...</p>
        </div>
      </div>
    );
  }

  // If user is on landing page or auth page, show minimal layout
  if (location === '/' || location === '/auth') {
    return <AppRouter />;
  }

  // For other authenticated routes, show full chat interface
  if (!user) {
    return <AppRouter />;
  }

  return (
    <div className="flex flex-col h-screen bg-neutral-50 text-neutral-700">
      <header className="bg-white border-b border-neutral-100 py-2 px-4 flex items-center justify-between shadow-sm">
        <div className="flex items-center">
          <SidebarButton onClick={() => setSidebarOpen(!sidebarOpen)} />
          <div className="flex items-center">
            <div className="h-8 w-8 bg-primary rounded-md flex items-center justify-center">
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
                className="text-white"
              >
                <path d="M12 8V4H8"></path>
                <rect width="16" height="12" x="4" y="8" rx="2"></rect>
                <path d="M2 14h2"></path>
                <path d="M20 14h2"></path>
                <path d="M15 13v2"></path>
                <path d="M9 13v2"></path>
              </svg>
            </div>
            <h1 className="ml-2 text-xl font-semibold text-neutral-700">
              Kultivate <span className="text-primary">AI</span>
            </h1>
          </div>
        </div>
        <div className="flex items-center space-x-3">
          <button 
            onClick={() => navigate('/settings')}
            className="text-neutral-500 hover:text-neutral-700"
          >
            <svg 
              xmlns="http://www.w3.org/2000/svg" 
              width="18" 
              height="18" 
              viewBox="0 0 24 24" 
              fill="none" 
              stroke="currentColor" 
              strokeWidth="2" 
              strokeLinecap="round" 
              strokeLinejoin="round" 
            >
              <path d="M12.22 2h-.44a2 2 0 0 0-2 2v.18a2 2 0 0 1-1 1.73l-.43.25a2 2 0 0 1-2 0l-.15-.08a2 2 0 0 0-2.73.73l-.22.38a2 2 0 0 0 .73 2.73l.15.1a2 2 0 0 1 1 1.72v.51a2 2 0 0 1-1 1.74l-.15.09a2 2 0 0 0-.73 2.73l.22.38a2 2 0 0 0 2.73.73l.15-.08a2 2 0 0 1 2 0l.43.25a2 2 0 0 1 1 1.73V20a2 2 0 0 0 2 2h.44a2 2 0 0 0 2-2v-.18a2 2 0 0 1 1-1.73l.43-.25a2 2 0 0 1 2 0l.15.08a2 2 0 0 0 2.73-.73l.22-.39a2 2 0 0 0-.73-2.73l-.15-.08a2 2 0 0 1-1-1.74v-.5a2 2 0 0 1 1-1.74l.15-.09a2 2 0 0 0 .73-2.73l-.22-.38a2 2 0 0 0-2.73-.73l-.15.08a2 2 0 0 1-2 0l-.43-.25a2 2 0 0 1-1-1.73V4a2 2 0 0 0-2-2z"></path>
              <circle cx="12" cy="12" r="3"></circle>
            </svg>
          </button>
          <div className="flex items-center gap-2">
            <div className="h-8 w-8 bg-neutral-100 rounded-full flex items-center justify-center">
              <svg 
                xmlns="http://www.w3.org/2000/svg" 
                width="16" 
                height="16" 
                viewBox="0 0 24 24" 
                fill="none" 
                stroke="currentColor" 
                strokeWidth="2" 
                strokeLinecap="round" 
                strokeLinejoin="round" 
                className="text-neutral-500"
              >
                <path d="M19 21v-2a4 4 0 0 0-4-4H9a4 4 0 0 0-4 4v2"></path>
                <circle cx="12" cy="7" r="4"></circle>
              </svg>
            </div>
            <HeaderLogoutButton />
          </div>
        </div>
      </header>

      <div className="flex flex-1 overflow-hidden">
        <Sidebar 
          isOpen={sidebarOpen} 
          onClose={() => setSidebarOpen(false)}
          onNewChat={handleNewChat}
        />
        <main className="flex-1 flex flex-col overflow-hidden">
          <AppRouter />
        </main>
      </div>
    </div>
  );
}

function App() {
  return (
    <AuthProvider>
      <TooltipProvider>
        <AuthContent />
        <Toaster />
      </TooltipProvider>
    </AuthProvider>
  );
}

export default App;
