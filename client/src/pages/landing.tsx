import { useState } from "react";
import { useLocation } from "wouter";
import { Button } from "@/components/ui/button";
import { Card, CardContent } from "@/components/ui/card";
import { Badge } from "@/components/ui/badge";
import { ArrowRight, BarChart3, Database, MessageSquare, Zap, Shield, Users } from "lucide-react";
import { useAuth } from "@/hooks/use-auth";

export default function LandingPage() {
  const [, navigate] = useLocation();
  const { user } = useAuth();

  const handleGetStarted = () => {
    if (user) {
      navigate("/dashboard");
    } else {
      navigate("/auth");
    }
  };

  const features = [
    {
      icon: <MessageSquare className="h-6 w-6" />,
      title: "Natural Language Queries",
      description: "Ask questions about your data in plain English. No SQL knowledge required."
    },
    {
      icon: <Database className="h-6 w-6" />,
      title: "BigQuery Integration",
      description: "Direct connection to your BigQuery data warehouse with real-time analysis."
    },
    {
      icon: <BarChart3 className="h-6 w-6" />,
      title: "Instant Visualizations",
      description: "Get tables, charts, and insights from your data in seconds."
    },
    {
      icon: <Zap className="h-6 w-6" />,
      title: "AI-Powered Analysis",
      description: "Google Gemini 2.0 Flash provides intelligent data insights and recommendations."
    },
    {
      icon: <Shield className="h-6 w-6" />,
      title: "Secure & Private",
      description: "Enterprise-grade security with your data staying in your infrastructure."
    },
    {
      icon: <Users className="h-6 w-6" />,
      title: "Team Collaboration",
      description: "Share insights and collaborate on data analysis with your team."
    }
  ];

  const howItWorks = [
    {
      step: "1",
      title: "Connect Your Data",
      description: "Link your Keboola Cloud project and BigQuery workspace to Kultivate AI."
    },
    {
      step: "2",
      title: "Ask Natural Questions",
      description: "Type questions like 'Show me last month's sales data' or 'Which products are trending?'"
    },
    {
      step: "3",
      title: "Get Instant Insights",
      description: "Receive data tables, visualizations, and AI-powered analysis immediately."
    }
  ];

  return (
    <div className="min-h-screen bg-gradient-to-br from-blue-50 via-white to-indigo-50">
      {/* Header */}
      <header className="bg-white/80 backdrop-blur-sm border-b border-gray-200 sticky top-0 z-50">
        <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8">
          <div className="flex justify-between items-center py-4">
            <div className="flex items-center">
              <div className="h-10 w-10 bg-gradient-to-r from-blue-600 to-indigo-600 rounded-lg flex items-center justify-center">
                <BarChart3 className="h-6 w-6 text-white" />
              </div>
              <h1 className="ml-3 text-2xl font-bold bg-gradient-to-r from-blue-600 to-indigo-600 bg-clip-text text-transparent">
                Kultivate AI
              </h1>
            </div>
            <div className="flex items-center space-x-4">
              {user ? (
                <>
                  <Button variant="ghost" onClick={() => navigate("/dashboard")}>
                    Dashboard
                  </Button>
                  <Button onClick={handleGetStarted} className="bg-gradient-to-r from-blue-600 to-indigo-600">
                    Open App
                  </Button>
                </>
              ) : (
                <>
                  <Button variant="ghost" onClick={() => navigate("/auth")}>
                    Log In
                  </Button>
                  <Button onClick={handleGetStarted} className="bg-gradient-to-r from-blue-600 to-indigo-600">
                    Get Started
                  </Button>
                </>
              )}
            </div>
          </div>
        </div>
      </header>

      {/* Hero Section */}
      <section className="py-20 px-4 sm:px-6 lg:px-8">
        <div className="max-w-7xl mx-auto text-center">
          <Badge variant="secondary" className="mb-8 bg-blue-100 text-blue-800 border-blue-200">
            🚀 Powered by Google Gemini 2.0 Flash
          </Badge>
          <h1 className="text-4xl md:text-6xl font-bold text-gray-900 mb-6">
            Your AI-Powered
            <span className="bg-gradient-to-r from-blue-600 to-indigo-600 bg-clip-text text-transparent block">
              Data Assistant
            </span>
          </h1>
          <p className="text-xl text-gray-600 mb-8 max-w-3xl mx-auto">
            Transform your BigQuery data into actionable insights with natural language queries. 
            No SQL required – just ask questions and get instant answers.
          </p>
          <div className="flex flex-col sm:flex-row gap-4 justify-center items-center">
            <Button 
              size="lg" 
              onClick={handleGetStarted}
              className="bg-gradient-to-r from-blue-600 to-indigo-600 hover:from-blue-700 hover:to-indigo-700 text-lg px-8 py-3"
            >
              {user ? "Open Dashboard" : "Start Analyzing Data"}
              <ArrowRight className="ml-2 h-5 w-5" />
            </Button>
            <Button variant="outline" size="lg" className="text-lg px-8 py-3">
              Watch Demo
            </Button>
          </div>
        </div>
      </section>

      {/* Features Section */}
      <section className="py-20 px-4 sm:px-6 lg:px-8 bg-white">
        <div className="max-w-7xl mx-auto">
          <div className="text-center mb-16">
            <h2 className="text-3xl md:text-4xl font-bold text-gray-900 mb-4">
              Powerful Features for Data Teams
            </h2>
            <p className="text-xl text-gray-600 max-w-2xl mx-auto">
              Everything you need to analyze, visualize, and understand your data.
            </p>
          </div>
          <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-8">
            {features.map((feature, index) => (
              <Card key={index} className="border-0 shadow-lg hover:shadow-xl transition-shadow">
                <CardContent className="p-6">
                  <div className="h-12 w-12 bg-gradient-to-r from-blue-600 to-indigo-600 rounded-lg flex items-center justify-center text-white mb-4">
                    {feature.icon}
                  </div>
                  <h3 className="text-xl font-semibold text-gray-900 mb-2">
                    {feature.title}
                  </h3>
                  <p className="text-gray-600">
                    {feature.description}
                  </p>
                </CardContent>
              </Card>
            ))}
          </div>
        </div>
      </section>

      {/* How It Works Section */}
      <section className="py-20 px-4 sm:px-6 lg:px-8">
        <div className="max-w-7xl mx-auto">
          <div className="text-center mb-16">
            <h2 className="text-3xl md:text-4xl font-bold text-gray-900 mb-4">
              How Kultivate AI Works
            </h2>
            <p className="text-xl text-gray-600 max-w-2xl mx-auto">
              Get from data to insights in three simple steps.
            </p>
          </div>
          <div className="grid grid-cols-1 md:grid-cols-3 gap-8">
            {howItWorks.map((item, index) => (
              <div key={index} className="text-center">
                <div className="h-16 w-16 bg-gradient-to-r from-blue-600 to-indigo-600 rounded-full flex items-center justify-center text-white text-2xl font-bold mx-auto mb-6">
                  {item.step}
                </div>
                <h3 className="text-xl font-semibold text-gray-900 mb-2">
                  {item.title}
                </h3>
                <p className="text-gray-600">
                  {item.description}
                </p>
              </div>
            ))}
          </div>
        </div>
      </section>

      {/* Example Queries Section */}
      <section className="py-20 px-4 sm:px-6 lg:px-8 bg-gray-50">
        <div className="max-w-7xl mx-auto">
          <div className="text-center mb-16">
            <h2 className="text-3xl md:text-4xl font-bold text-gray-900 mb-4">
              Ask Questions Like These
            </h2>
            <p className="text-xl text-gray-600 max-w-2xl mx-auto">
              Natural language queries that work with your data.
            </p>
          </div>
          <div className="grid grid-cols-1 md:grid-cols-2 gap-6 max-w-4xl mx-auto">
            {[
              "Show me sales data from last quarter",
              "Which customers have the highest lifetime value?", 
              "What are the trending products this month?",
              "Show me user engagement metrics by region"
            ].map((query, index) => (
              <Card key={index} className="p-6 bg-white border-l-4 border-l-blue-600">
                <p className="text-gray-700 italic">"{query}"</p>
              </Card>
            ))}
          </div>
        </div>
      </section>

      {/* CTA Section */}
      <section className="py-20 px-4 sm:px-6 lg:px-8 bg-gradient-to-r from-blue-600 to-indigo-600">
        <div className="max-w-4xl mx-auto text-center">
          <h2 className="text-3xl md:text-4xl font-bold text-white mb-4">
            Ready to Transform Your Data Analysis?
          </h2>
          <p className="text-xl text-blue-100 mb-8">
            Join teams already using Kultivate AI to make data-driven decisions faster.
          </p>
          <Button 
            size="lg" 
            onClick={handleGetStarted}
            className="bg-white text-blue-600 hover:bg-gray-100 text-lg px-8 py-3"
          >
            {user ? "Open Dashboard" : "Get Started Today"}
            <ArrowRight className="ml-2 h-5 w-5" />
          </Button>
        </div>
      </section>

      {/* Footer */}
      <footer className="bg-gray-900 text-white py-12 px-4 sm:px-6 lg:px-8">
        <div className="max-w-7xl mx-auto">
          <div className="flex items-center justify-between">
            <div className="flex items-center">
              <div className="h-8 w-8 bg-gradient-to-r from-blue-600 to-indigo-600 rounded-lg flex items-center justify-center">
                <BarChart3 className="h-5 w-5 text-white" />
              </div>
              <span className="ml-2 text-xl font-bold">Kultivate AI</span>
            </div>
            <p className="text-gray-400">
              © 2025 Kultivate AI. AI-powered data insights.
            </p>
          </div>
        </div>
      </footer>
    </div>
  );
}