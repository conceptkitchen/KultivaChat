import React, { useState } from "react";
import { Button } from "@/components/ui/button";
import { AssistantAI } from "@/components/ui/assistant-ai";
import { useLocation } from "wouter";
import { useMutation } from "@tanstack/react-query";
import { apiRequest } from "@/lib/queryClient";
import { queryClient } from "@/lib/queryClient";
import { GeminiTester } from "@/components/gemini-tester";

export default function Home() {
  const [, navigate] = useLocation();

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

  const startNewChat = () => {
    createConversationMutation.mutate();
  };

  const [showTester, setShowTester] = useState(false);

  return (
    <div className="flex flex-col items-center justify-center h-full p-4 md:p-8 bg-neutral-50">
      <div className="max-w-2xl w-full text-center space-y-8">
        <div className="flex flex-col items-center justify-center space-y-4">
          <AssistantAI size="lg" />
          <h1 className="text-3xl font-bold text-neutral-800">
            Welcome to Kultivate <span className="text-primary">AI</span>
          </h1>
          <p className="text-neutral-600 max-w-md">
            Your AI assistant for data visualization, code generation, 
            and Keboola API integration
          </p>
          <Button 
            variant="outline"
            onClick={() => setShowTester(!showTester)}
          >
            {showTester ? "Hide Gemini Tester" : "Test Gemini 2.0 Flash"}
          </Button>
        </div>
        
        {showTester && <GeminiTester />}

        <div className="grid gap-4 md:grid-cols-2 p-4">
          <div className="bg-white rounded-lg p-4 shadow-sm border border-neutral-200">
            <h2 className="font-medium text-lg text-neutral-800 mb-2">
              Data Visualization
            </h2>
            <p className="text-neutral-600 text-sm mb-3">
              Transform your data into insightful charts, tables, and interactive visualizations
            </p>
            <Button 
              className="w-full mt-2"
              onClick={() => {
                startNewChat();
              }}
            >
              Create Visualization
            </Button>
          </div>

          <div className="bg-white rounded-lg p-4 shadow-sm border border-neutral-200">
            <h2 className="font-medium text-lg text-neutral-800 mb-2">
              Keboola Integration
            </h2>
            <p className="text-neutral-600 text-sm mb-3">
              Connect to the Keboola API for data extraction, transformation, and loading
            </p>
            <Button 
              className="w-full mt-2"
              onClick={() => {
                startNewChat();
              }}
            >
              Set Up Integration
            </Button>
          </div>

          <div className="bg-white rounded-lg p-4 shadow-sm border border-neutral-200">
            <h2 className="font-medium text-lg text-neutral-800 mb-2">
              Code Generation
            </h2>
            <p className="text-neutral-600 text-sm mb-3">
              Generate code snippets for data manipulation, API calls, and more
            </p>
            <Button 
              className="w-full mt-2"
              onClick={() => {
                startNewChat();
              }}
            >
              Generate Code
            </Button>
          </div>

          <div className="bg-white rounded-lg p-4 shadow-sm border border-neutral-200">
            <h2 className="font-medium text-lg text-neutral-800 mb-2">
              Documentation
            </h2>
            <p className="text-neutral-600 text-sm mb-3">
              Create comprehensive documentation for your data pipelines and processes
            </p>
            <Button 
              className="w-full mt-2"
              onClick={() => {
                startNewChat();
              }}
            >
              Create Documentation
            </Button>
          </div>
        </div>

        <div className="flex justify-center">
          <Button 
            size="lg"
            onClick={startNewChat}
            disabled={createConversationMutation.isPending}
            className="bg-primary hover:bg-primary-dark text-white transition-colors"
          >
            {createConversationMutation.isPending ? "Creating..." : "Start a New Chat"}
          </Button>
        </div>
      </div>
    </div>
  );
}
