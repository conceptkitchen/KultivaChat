import React from "react";
import { Card, CardContent, CardHeader, CardTitle } from "@/components/ui/card";
import { Button } from "@/components/ui/button";
import { Copy, Download, Expand } from "lucide-react";
import { Tabs, TabsContent, TabsList, TabsTrigger } from "@/components/ui/tabs";
import { Separator } from "@/components/ui/separator";
import { DisplayContent } from "@/lib/utils";
import { cn } from "@/lib/utils";
import { useToast } from "@/hooks/use-toast";
import {
  Table,
  TableBody,
  TableCell,
  TableHead,
  TableHeader,
  TableRow,
} from "@/components/ui/table";
import { ScrollArea } from "@/components/ui/scroll-area";
import { BarChart, Bar, XAxis, YAxis, CartesianGrid, Tooltip, ResponsiveContainer } from "recharts";

interface CanvasDisplayProps {
  displays: DisplayContent[];
}

export function CanvasDisplay({ displays }: CanvasDisplayProps) {
  const { toast } = useToast();
  
  if (!displays || displays.length === 0) return null;

  const handleCopy = (content: string) => {
    navigator.clipboard.writeText(content)
      .then(() => {
        toast({
          title: "Copied to clipboard",
          description: "Content has been copied to your clipboard",
        });
      })
      .catch((err) => {
        toast({
          title: "Failed to copy",
          description: "Could not copy content to clipboard",
          variant: "destructive",
        });
      });
  };

  const renderContent = (display: DisplayContent) => {
    switch (display.type) {
      case "text":
        return (
          <div className="text-sm text-neutral-800 whitespace-pre-wrap">
            {display.content as string}
          </div>
        );
      
      case "code":
        return (
          <div className="relative">
            <pre className="p-4 overflow-x-auto font-mono text-sm bg-neutral-800 text-neutral-100 rounded-md">
              <code>{display.content as string}</code>
            </pre>
            <Button
              variant="ghost"
              size="sm"
              className="absolute top-2 right-2 text-neutral-300 hover:text-white"
              onClick={() => handleCopy(display.content as string)}
            >
              <Copy className="h-4 w-4 mr-1" />
              Copy
            </Button>
          </div>
        );
      
      case "table":
        const tableData = display.content as Record<string, any>[];
        if (!tableData.length) return <div>No data to display</div>;
        
        const columns = Object.keys(tableData[0]);
        
        return (
          <div className="border rounded-md">
            <ScrollArea className="max-h-80">
              <Table>
                <TableHeader>
                  <TableRow>
                    {columns.map((column) => (
                      <TableHead key={column} className="whitespace-nowrap">
                        {column}
                      </TableHead>
                    ))}
                    <TableHead>Actions</TableHead>
                  </TableRow>
                </TableHeader>
                <TableBody>
                  {tableData.map((row, i) => (
                    <TableRow key={i} className={i % 2 === 0 ? "" : "bg-neutral-50"}>
                      {columns.map((column) => (
                        <TableCell key={column} className="whitespace-nowrap">
                          {row[column]}
                        </TableCell>
                      ))}
                      <TableCell>
                        <Button variant="ghost" size="sm" className="text-primary hover:text-primary-dark">
                          <svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2" strokeLinecap="round" strokeLinejoin="round" className="lucide lucide-eye"><path d="M2 12s3-7 10-7 10 7 10 7-3 7-10 7-10-7-10-7Z"/><circle cx="12" cy="12" r="3"/></svg>
                        </Button>
                        <Button variant="ghost" size="sm" className="text-neutral-500 hover:text-neutral-700">
                          <svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2" strokeLinecap="round" strokeLinejoin="round" className="lucide lucide-pencil"><path d="M17 3a2.85 2.83 0 1 1 4 4L7.5 20.5 2 22l1.5-5.5Z"/><path d="m15 5 4 4"/></svg>
                        </Button>
                      </TableCell>
                    </TableRow>
                  ))}
                </TableBody>
              </Table>
            </ScrollArea>
            <div className="flex items-center justify-between p-4 border-t border-neutral-200">
              <div className="flex items-center space-x-2">
                <Button variant="outline" size="sm">Previous</Button>
                <span className="text-sm text-neutral-500">Page 1 of 1</span>
                <Button variant="outline" size="sm">Next</Button>
              </div>
              <Button size="sm">Export CSV</Button>
            </div>
          </div>
        );

      case "visualization":
        const chartData = display.content as Record<string, any>[];
        return (
          <div className="w-full h-80">
            <ResponsiveContainer width="100%" height="100%">
              <BarChart data={chartData}
                margin={{ top: 20, right: 30, left: 20, bottom: 5 }}>
                <CartesianGrid strokeDasharray="3 3" />
                <XAxis dataKey="name" />
                <YAxis />
                <Tooltip />
                <Bar dataKey="value" fill="#E9BE00" />
              </BarChart>
            </ResponsiveContainer>
          </div>
        );

      case "documentation":
        return (
          <div className="prose prose-sm max-w-none">
            <div dangerouslySetInnerHTML={{ __html: display.content as string }} />
          </div>
        );

      default:
        return <div>Unknown content type</div>;
    }
  };

  return (
    <div className="space-y-4">
      {displays.map((display, index) => (
        <Card key={index} className="shadow-sm border-neutral-200 overflow-hidden">
          <CardHeader className="bg-secondary px-4 py-2 flex flex-row items-center justify-between">
            <CardTitle className="text-white text-sm font-medium">
              {display.title || (display.type.charAt(0).toUpperCase() + display.type.slice(1))}
            </CardTitle>
            <div className="flex space-x-2">
              {display.type === "code" && (
                <Button variant="ghost" size="sm" className="text-neutral-300 hover:text-white text-xs py-1 px-2 h-auto">
                  <Copy className="h-3 w-3 mr-1" />
                  Copy
                </Button>
              )}
              {(display.type === "table" || display.type === "visualization") && (
                <Button variant="ghost" size="sm" className="text-neutral-300 hover:text-white text-xs py-1 px-2 h-auto">
                  <Expand className="h-3 w-3 mr-1" />
                  Expand
                </Button>
              )}
              {(display.type === "table" || display.type === "documentation" || display.type === "code") && (
                <Button variant="ghost" size="sm" className="text-neutral-300 hover:text-white text-xs py-1 px-2 h-auto">
                  <Download className="h-3 w-3 mr-1" />
                  Download
                </Button>
              )}
            </div>
          </CardHeader>
          <CardContent className={cn(
            "p-0",
            display.type === "table" ? "bg-white" : "p-4"
          )}>
            {renderContent(display)}
          </CardContent>
        </Card>
      ))}
    </div>
  );
}
