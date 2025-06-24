import { useState } from "react";
import { useAuth } from "@/hooks/useAuth";
import { Button } from "@/components/ui/button";
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card";
import { Tabs, TabsContent, TabsList, TabsTrigger } from "@/components/ui/tabs";
import { Badge } from "@/components/ui/badge";
import { 
  BarChart3, 
  MessageSquare, 
  Database, 
  TrendingUp, 
  Users, 
  Calendar,
  Settings,
  LogOut
} from "lucide-react";
import ChatPage from "./chat";

export default function Dashboard() {
  const { user, isLoading } = useAuth();
  const [activeTab, setActiveTab] = useState("overview");

  if (isLoading) {
    return (
      <div className="flex items-center justify-center min-h-screen">
        <div className="animate-spin rounded-full h-12 w-12 border-b-2 border-primary"></div>
      </div>
    );
  }

  return (
    <div className="min-h-screen bg-gray-50">
      {/* Top Navigation */}
      <nav className="bg-white border-b border-gray-200">
        <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8">
          <div className="flex justify-between items-center py-4">
            <div className="flex items-center">
              <div className="h-8 w-8 bg-gradient-to-br from-blue-600 to-purple-600 rounded-lg flex items-center justify-center">
                <Database className="h-5 w-5 text-white" />
              </div>
              <h1 className="ml-3 text-xl font-bold text-gray-900">
                Kultivate <span className="text-blue-600">AI</span>
              </h1>
            </div>
            
            <div className="flex items-center space-x-4">
              <div className="flex items-center space-x-2">
                {user?.profileImageUrl && (
                  <img 
                    src={user.profileImageUrl} 
                    alt="Profile" 
                    className="h-8 w-8 rounded-full object-cover"
                  />
                )}
                <span className="text-sm font-medium text-gray-700">
                  {user?.firstName || user?.email}
                </span>
              </div>
              <Button 
                variant="ghost" 
                size="sm"
                onClick={() => window.location.href = "/api/logout"}
              >
                <LogOut className="h-4 w-4" />
              </Button>
            </div>
          </div>
        </div>
      </nav>

      <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
        <div className="mb-8">
          <h2 className="text-2xl font-bold text-gray-900 mb-2">
            Welcome back, {user?.firstName || 'there'}!
          </h2>
          <p className="text-gray-600">
            Your AI-powered data analysis dashboard
          </p>
        </div>

        <Tabs value={activeTab} onValueChange={setActiveTab} className="space-y-6">
          <TabsList className="grid w-full grid-cols-4">
            <TabsTrigger value="overview">Overview</TabsTrigger>
            <TabsTrigger value="analytics">Analytics</TabsTrigger>
            <TabsTrigger value="chat">AI Assistant</TabsTrigger>
            <TabsTrigger value="settings">Settings</TabsTrigger>
          </TabsList>

          <TabsContent value="overview" className="space-y-6">
            {/* Quick Stats */}
            <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-6">
              <Card>
                <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
                  <CardTitle className="text-sm font-medium">
                    Data Sources
                  </CardTitle>
                  <Database className="h-4 w-4 text-muted-foreground" />
                </CardHeader>
                <CardContent>
                  <div className="text-2xl font-bold">64</div>
                  <p className="text-xs text-muted-foreground">
                    Connected tables
                  </p>
                </CardContent>
              </Card>

              <Card>
                <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
                  <CardTitle className="text-sm font-medium">
                    AI Conversations
                  </CardTitle>
                  <MessageSquare className="h-4 w-4 text-muted-foreground" />
                </CardHeader>
                <CardContent>
                  <div className="text-2xl font-bold">112+</div>
                  <p className="text-xs text-muted-foreground">
                    This month
                  </p>
                </CardContent>
              </Card>

              <Card>
                <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
                  <CardTitle className="text-sm font-medium">
                    Insights Generated
                  </CardTitle>
                  <TrendingUp className="h-4 w-4 text-muted-foreground" />
                </CardHeader>
                <CardContent>
                  <div className="text-2xl font-bold">847</div>
                  <p className="text-xs text-muted-foreground">
                    +12% from last month
                  </p>
                </CardContent>
              </Card>

              <Card>
                <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
                  <CardTitle className="text-sm font-medium">
                    Active Users
                  </CardTitle>
                  <Users className="h-4 w-4 text-muted-foreground" />
                </CardHeader>
                <CardContent>
                  <div className="text-2xl font-bold">1</div>
                  <p className="text-xs text-muted-foreground">
                    Current session
                  </p>
                </CardContent>
              </Card>
            </div>

            {/* Recent Activity */}
            <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
              <Card>
                <CardHeader>
                  <CardTitle>Recent Data Queries</CardTitle>
                  <CardDescription>
                    Your latest AI-powered data analysis
                  </CardDescription>
                </CardHeader>
                <CardContent className="space-y-4">
                  <div className="flex items-center space-x-4">
                    <Badge variant="secondary">Attendees</Badge>
                    <span className="text-sm text-gray-600">Undiscovered events analysis</span>
                    <span className="text-xs text-gray-400 ml-auto">2 min ago</span>
                  </div>
                  <div className="flex items-center space-x-4">
                    <Badge variant="outline">Orders</Badge>
                    <span className="text-sm text-gray-600">Kapwa Gardens sales data</span>
                    <span className="text-xs text-gray-400 ml-auto">1 hour ago</span>
                  </div>
                  <div className="flex items-center space-x-4">
                    <Badge variant="secondary">Vendors</Badge>
                    <span className="text-sm text-gray-600">Vendor performance metrics</span>
                    <span className="text-xs text-gray-400 ml-auto">3 hours ago</span>
                  </div>
                </CardContent>
              </Card>

              <Card>
                <CardHeader>
                  <CardTitle>Data Sources</CardTitle>
                  <CardDescription>
                    Connected Keboola workspace tables
                  </CardDescription>
                </CardHeader>
                <CardContent>
                  <div className="space-y-3">
                    <div className="flex items-center justify-between">
                      <span className="text-sm">Undiscovered Events</span>
                      <Badge variant="default">16 tables</Badge>
                    </div>
                    <div className="flex items-center justify-between">
                      <span className="text-sm">Kapwa Gardens</span>
                      <Badge variant="default">24 tables</Badge>
                    </div>
                    <div className="flex items-center justify-between">
                      <span className="text-sm">Balay Kreative</span>
                      <Badge variant="default">12 tables</Badge>
                    </div>
                    <div className="flex items-center justify-between">
                      <span className="text-sm">Close-Out Sales</span>
                      <Badge variant="default">12 tables</Badge>
                    </div>
                  </div>
                </CardContent>
              </Card>
            </div>
          </TabsContent>

          <TabsContent value="analytics" className="space-y-6">
            <Card>
              <CardHeader>
                <CardTitle>Data Analytics</CardTitle>
                <CardDescription>
                  Advanced analytics and visualization tools (Coming Soon)
                </CardDescription>
              </CardHeader>
              <CardContent className="text-center py-12">
                <BarChart3 className="h-16 w-16 text-gray-400 mx-auto mb-4" />
                <h3 className="text-lg font-medium text-gray-900 mb-2">
                  Advanced Analytics Dashboard
                </h3>
                <p className="text-gray-600 mb-6">
                  Rich visualizations, custom charts, and automated reporting features are coming soon.
                </p>
                <Button variant="outline">
                  Request Early Access
                </Button>
              </CardContent>
            </Card>
          </TabsContent>

          <TabsContent value="chat" className="space-y-6">
            <Card className="h-[600px]">
              <CardHeader>
                <CardTitle>AI Data Assistant</CardTitle>
                <CardDescription>
                  Ask questions about your data in natural language
                </CardDescription>
              </CardHeader>
              <CardContent className="p-0 h-full">
                <div className="h-full">
                  <ChatPage />
                </div>
              </CardContent>
            </Card>
          </TabsContent>

          <TabsContent value="settings" className="space-y-6">
            <Card>
              <CardHeader>
                <CardTitle>Account Settings</CardTitle>
                <CardDescription>
                  Manage your account preferences and data connections
                </CardDescription>
              </CardHeader>
              <CardContent className="space-y-6">
                <div>
                  <h4 className="text-sm font-medium mb-2">Profile Information</h4>
                  <div className="space-y-2">
                    <p className="text-sm text-gray-600">Email: {user?.email}</p>
                    <p className="text-sm text-gray-600">Name: {user?.firstName} {user?.lastName}</p>
                  </div>
                </div>
                
                <div>
                  <h4 className="text-sm font-medium mb-2">Data Connections</h4>
                  <div className="flex items-center justify-between p-3 bg-green-50 rounded-lg">
                    <div>
                      <p className="text-sm font-medium text-green-800">Keboola Workspace</p>
                      <p className="text-xs text-green-600">Connected and active</p>
                    </div>
                    <Badge variant="secondary" className="bg-green-100 text-green-800">
                      Connected
                    </Badge>
                  </div>
                </div>

                <div className="pt-4 border-t">
                  <Button 
                    variant="destructive" 
                    onClick={() => window.location.href = "/api/logout"}
                  >
                    <LogOut className="h-4 w-4 mr-2" />
                    Sign Out
                  </Button>
                </div>
              </CardContent>
            </Card>
          </TabsContent>
        </Tabs>
      </div>
    </div>
  );
}