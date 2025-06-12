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
        
        // Handle empty or invalid table data
        if (!tableData || !Array.isArray(tableData)) {
          return (
            <div className="p-4 text-center text-gray-500 bg-gray-50 rounded-md">
              No valid table data available
            </div>
          );
        }
        
        if (tableData.length === 0) {
          return (
            <div className="p-4 text-center text-gray-500 bg-gray-50 rounded-md">
              Table is empty - no records found
            </div>
          );
        }
        
        // Get columns from first row, handle case where first row might be empty
        let columns: string[] = [];
        for (const row of tableData) {
          if (row && typeof row === 'object' && Object.keys(row).length > 0) {
            columns = Object.keys(row);
            break;
          }
        }
        
        if (columns.length === 0) {
          return (
            <div className="p-4 text-center text-gray-500 bg-gray-50 rounded-md">
              Table data structure is invalid
            </div>
          );
        }
        
        return (
          <div className="border rounded-md w-full overflow-hidden">
            <div className="overflow-x-auto overflow-y-auto max-h-96 bg-white">
              <Table className="w-full min-w-max">
                <TableHeader className="sticky top-0 bg-gray-50 z-10">
                  <TableRow>
                    {columns.map((column) => (
                      <TableHead key={column} className="px-4 py-3 text-left font-semibold text-xs uppercase tracking-wider text-gray-700 border-b whitespace-nowrap min-w-32">
                        {column}
                      </TableHead>
                    ))}
                  </TableRow>
                </TableHeader>
                <TableBody>
                  {tableData.map((row, i) => (
                    <TableRow key={i} className={i % 2 === 0 ? "bg-white hover:bg-gray-50" : "bg-gray-50 hover:bg-gray-100"}>
                      {columns.map((column) => (
                        <TableCell key={column} className="px-4 py-3 text-sm text-gray-900 border-b border-gray-200">
                          <div className="min-w-0 break-words" title={String(row[column] || '')}>
                            {String(row[column] || '')}
                          </div>
                        </TableCell>
                      ))}
                    </TableRow>
                  ))}
                </TableBody>
              </Table>
            </div>
            <div className="flex items-center justify-between p-3 border-t border-neutral-200 bg-gray-50">
              <span className="text-sm text-neutral-600 font-medium">
                Showing {tableData.length} {tableData.length === 1 ? 'record' : 'records'} from your Keboola workspace
              </span>
              <div className="flex space-x-2">
                <Button 
                  variant="outline" 
                  size="sm"
                  onClick={() => handleCopy(JSON.stringify(tableData, null, 2))}
                  className="text-xs"
                >
                  <Copy className="h-3 w-3 mr-1" />
                  Copy Data
                </Button>
              </div>
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
