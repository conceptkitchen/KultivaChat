import React from "react";
import { DisplayContent } from "@/lib/utils";
import { Copy } from "lucide-react";

interface CanvasDisplayProps {
  displays: DisplayContent[];
}

export function CanvasDisplay({ displays }: CanvasDisplayProps) {
  console.log('CanvasDisplay received displays:', displays?.length || 0);
  
  if (!displays || displays.length === 0) {
    console.log('No displays to render');
    return null;
  }

  const handleCopy = (content: string) => {
    navigator.clipboard.writeText(content);
  };

  const renderDisplay = (display: DisplayContent, index: number) => {
    console.log(`Rendering display ${index}:`, display.type, 'with content length:', display.content?.length || 0);
    
    if (display.type === "table") {
      const tableData = display.content as Record<string, any>[];
      
      if (!tableData || !Array.isArray(tableData) || tableData.length === 0) {
        console.log('No table data found for display', index);
        return (
          <div key={index} className="p-4 text-center text-gray-500 bg-gray-50 rounded-md">
            No table data available
          </div>
        );
      }
      
      console.log(`Table has ${tableData.length} rows`);
      console.log('First row keys:', Object.keys(tableData[0] || {}));

      const columns = Object.keys(tableData[0] || {});
      
      return (
        <div key={index} className="border rounded-lg overflow-hidden bg-white shadow-sm">
          <div className="bg-gray-800 px-4 py-3">
            <h3 className="text-white font-semibold text-sm">
              {display.title || "Available Data Tables"}
            </h3>
          </div>
          <div className="overflow-auto max-h-96">
            <table className="w-full">
              <thead className="bg-gray-50 sticky top-0">
                <tr>
                  {columns.map((column) => (
                    <th key={column} className="px-4 py-3 text-left text-xs font-semibold text-gray-700 uppercase tracking-wider border-b">
                      {column.replace(/_/g, ' ')}
                    </th>
                  ))}
                </tr>
              </thead>
              <tbody className="bg-white">
                {tableData.map((row, i) => (
                  <tr key={i} className={i % 2 === 0 ? "bg-white" : "bg-gray-50"}>
                    {columns.map((column) => (
                      <td key={column} className="px-4 py-3 text-sm text-gray-900 border-b border-gray-200">
                        <div className="break-words max-w-xs">
                          {String(row[column] || '')}
                        </div>
                      </td>
                    ))}
                  </tr>
                ))}
              </tbody>
            </table>
          </div>
          <div className="bg-gray-50 px-4 py-3 border-t flex justify-between items-center">
            <span className="text-sm text-gray-600">
              {tableData.length} tables from your Kapwa Gardens workspace
            </span>
            <button 
              onClick={() => handleCopy(JSON.stringify(tableData, null, 2))}
              className="text-xs px-3 py-1 bg-white border rounded hover:bg-gray-50 flex items-center"
            >
              <Copy className="h-3 w-3 mr-1" />
              Copy
            </button>
          </div>
        </div>
      );
    }

    // Handle other display types
    return (
      <div key={index} className="p-4 bg-gray-100 rounded-md">
        <div className="text-sm">{display.type}: {String(display.content)}</div>
      </div>
    );
  };

  return (
    <div className="space-y-4">
      {displays.map((display, index) => renderDisplay(display, index))}
    </div>
  );
}
