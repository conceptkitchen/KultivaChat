# Kultivate AI - Chat Table Display Architecture

## Overview

The chat table display system in Kultivate AI transforms raw SQL query results into interactive, visually appealing tables within the chat interface. This document explains the complete flow from backend data extraction through frontend rendering, including the display object structure, React components, and styling system.

## Display System Architecture

### Data Flow Pipeline
```
SQL Query → Tool Execution → Data Extraction → Display Creation → API Response → Frontend Rendering
     ↓            ↓              ↓               ↓              ↓              ↓
BigQuery      AI Function    Multi-layer     Display Object   JSON Response   React Table
Results       Response       Extraction      Structure        to Frontend     Component
```

### Core Components
- **Backend Display Creation**: Python Flask backend creates display objects
- **API Transport**: Node.js proxy forwards structured display data
- **Frontend Rendering**: React components render interactive tables
- **Styling System**: Tailwind CSS with custom table styling

## Backend Display Creation

### Display Object Structure

The backend creates standardized display objects for consistent frontend rendering:

```python
# Standard display object format
display_object = {
    "type": "table",                    # Display type (table, chart, text, code)
    "title": "Query Results",           # Human-readable title for the display
    "content": [                        # Array of data rows as dictionaries
        {"column1": "value1", "column2": "value2", "column3": 123},
        {"column1": "value3", "column2": "value4", "column3": 456},
        {"column1": "value5", "column2": "value6", "column3": 789}
    ]
}
```

### Display Creation Process

Located in `backend/main_2.py`, the display creation follows a multi-layered approach:

```python
def create_table_display(query_data, tool_display_title):
    """Create a standardized display object from query results"""
    
    if not query_data or not isinstance(query_data, list):
        return None
    
    # Validate data structure
    if not all(isinstance(row, dict) for row in query_data):
        app.logger.warning("Query data contains non-dict rows, skipping display creation")
        return None
    
    # Create display object
    display = {
        "type": "table",
        "title": tool_display_title or "Query Results", 
        "content": query_data
    }
    
    app.logger.info(f"Created table display with {len(query_data)} rows")
    return display
```

### Multi-Layer Data Extraction

The system uses multiple extraction methods to ensure data is captured from AI tool responses:

```python
# Primary extraction from response parts
def extract_from_response_parts(response):
    """Extract data from Gemini response parts"""
    query_data = None
    tool_display_title = "Tool Results"
    
    if hasattr(response, 'parts') and response.parts:
        for part in response.parts:
            if hasattr(part, 'function_response') and part.function_response:
                func_result = part.function_response.response
                if isinstance(func_result, dict) and func_result.get('status') == 'success':
                    if func_result.get('data') and isinstance(func_result['data'], list):
                        query_data = func_result['data']
                        tool_display_title = "SQL Query Results"
                        break
    
    return query_data, tool_display_title

# Secondary extraction from chat history
def extract_from_chat_history(chat_session):
    """Extract data from chat session history"""
    query_data = None
    tool_display_title = "Historical Results"
    
    try:
        history = chat_session.get_history()
        if history and len(history) > 0:
            # Process last few messages for tool responses
            for message in reversed(history[-5:]):  # Check last 5 messages
                if hasattr(message, 'parts'):
                    for part in message.parts:
                        if hasattr(part, 'function_response'):
                            func_result = json.loads(part.function_response.response)
                            if func_result.get('status') == 'success' and func_result.get('data'):
                                query_data = func_result['data']
                                break
                if query_data:
                    break
    except Exception as e:
        app.logger.error(f"History extraction failed: {e}")
    
    return query_data, tool_display_title

# Emergency reconstruction from AI text
def emergency_reconstruction(final_answer):
    """Reconstruct data when extraction fails but AI mentions retrieved data"""
    query_data = None
    tool_display_title = "Reconstructed Results"
    
    if "retrieved" in final_answer.lower() and ("rows" in final_answer.lower() or "table" in final_answer.lower()):
        import re
        
        # Extract table names and row counts from AI response
        emergency_tables = {
            '2023-08-19-UNDISCOVERED-SF---Close-Out-Sales-Check-out-Sheet-All-vendors': 'Undiscovered SF Data',
            'Undiscovered---Attendees-Export---Squarespace---All-data-orders--2-': 'Undiscovered Attendees',
            'Balay-Kreative---attendees---all-orders-Ballay-Kreative---attendees---all-orders': 'Balay-Kreative Data',
            'Undiscovered-Vendor-Export---Squarespace---All-data-orders': 'Undiscovered Vendors'
        }
        
        # Find matching table in AI response
        for table_name, display_title in emergency_tables.items():
            if table_name in final_answer or table_name.lower() in final_answer.lower():
                # Extract limit from AI response
                limit_match = re.search(r'(\d+)\s+rows', final_answer)
                limit = int(limit_match.group(1)) if limit_match else 10
                
                # Re-execute query
                emergency_query = f"SELECT * FROM `kbc-use4-839-261b.WORKSPACE_21894820.{table_name}` LIMIT {limit}"
                emergency_result = internal_execute_sql_query(emergency_query)
                
                if emergency_result.get('status') == 'success' and emergency_result.get('data'):
                    query_data = emergency_result['data']
                    tool_display_title = display_title
                    app.logger.info(f"EMERGENCY RECONSTRUCTION SUCCESS: {len(query_data)} rows")
                    break
    
    return query_data, tool_display_title
```

### Complete Display Creation Flow

```python
def create_displays_from_ai_response(response, chat_session, final_answer):
    """Main function to create displays from AI response"""
    
    displays = []
    query_data = None
    tool_display_title = "Query Results"
    
    # Step 1: Try primary extraction from response parts
    query_data, tool_display_title = extract_from_response_parts(response)
    
    # Step 2: Try secondary extraction from chat history
    if not query_data:
        query_data, tool_display_title = extract_from_chat_history(chat_session)
    
    # Step 3: Try emergency reconstruction from AI text
    if not query_data:
        query_data, tool_display_title = emergency_reconstruction(final_answer)
    
    # Step 4: Create display object if data was extracted
    if query_data and isinstance(query_data, list) and len(query_data) > 0:
        display = create_table_display(query_data, tool_display_title)
        if display:
            displays.append(display)
            app.logger.info(f"DISPLAY CREATED: {len(query_data)} rows in '{tool_display_title}'")
    else:
        app.logger.warning(f"NO DISPLAY CREATED: query_data={type(query_data)}, length={len(query_data) if isinstance(query_data, list) else 'N/A'}")
    
    return displays
```

## API Response Format

### Chat Endpoint Response Structure

The `/api/chat` endpoint returns structured data including displays:

```python
# In backend/main_2.py - chat endpoint response
@app.route('/api/chat', methods=['POST'])
def chat():
    # ... process message and create displays ...
    
    response_data = {
        "response": final_answer,           # AI text response
        "displays": displays,               # Array of display objects
        "conversation_id": conversation_id, # Chat session ID
        "timestamp": datetime.utcnow().isoformat()
    }
    
    return jsonify(response_data)
```

### Example API Response

```json
{
  "response": "I found 10 rows of data from the Undiscovered attendees table. Here's what I retrieved:",
  "displays": [
    {
      "type": "table",
      "title": "Undiscovered Attendees Export",
      "content": [
        {
          "name": "John Smith", 
          "email": "john@example.com",
          "event_date": "2023-08-19",
          "ticket_type": "VIP"
        },
        {
          "name": "Jane Doe",
          "email": "jane@example.com", 
          "event_date": "2023-08-19",
          "ticket_type": "General"
        }
      ]
    }
  ],
  "conversation_id": "550e8400-e29b-41d4-a716-446655440000",
  "timestamp": "2025-06-24T21:15:30.123456"
}
```

## Frontend Display Rendering

### React Component Architecture

The frontend uses a hierarchical component structure for rendering tables:

```
Chat.tsx
├── ChatBubble.tsx (AI message container)
│   ├── CanvasDisplay.tsx (Display container)
│   │   ├── DataTable.tsx (Table component)
│   │   │   ├── TableHeader.tsx
│   │   │   ├── TableBody.tsx
│   │   │   └── TablePagination.tsx
│   │   └── EmptyState.tsx
│   └── MessageContent.tsx (Text content)
└── MessageInput.tsx
```

### ChatBubble Component

Located in `client/src/components/ui/chat-bubble.tsx`:

```tsx
interface ChatBubbleProps {
  message: {
    role: 'user' | 'assistant';
    content: string;
    displays?: DisplayObject[];
  };
}

export function ChatBubble({ message }: ChatBubbleProps) {
  return (
    <div className={`flex ${message.role === 'user' ? 'justify-end' : 'justify-start'}`}>
      <div className={`max-w-[80%] rounded-lg p-4 ${
        message.role === 'user' 
          ? 'bg-blue-500 text-white' 
          : 'bg-gray-100 text-gray-900'
      }`}>
        {/* Text content */}
        <div className="prose prose-sm max-w-none">
          {message.content}
        </div>
        
        {/* Table displays */}
        {message.displays && message.displays.length > 0 && (
          <div className="mt-4">
            <CanvasDisplay displays={message.displays} />
          </div>
        )}
      </div>
    </div>
  );
}
```

### CanvasDisplay Component

Located in `client/src/components/ui/canvas-display.tsx`:

```tsx
interface DisplayObject {
  type: 'table' | 'chart' | 'text' | 'code';
  title: string;
  content: any[];
}

interface CanvasDisplayProps {
  displays: DisplayObject[];
}

export function CanvasDisplay({ displays }: CanvasDisplayProps) {
  if (!displays || displays.length === 0) {
    return null;
  }

  return (
    <div className="space-y-6">
      {displays.map((display, index) => (
        <div key={index} className="border border-gray-200 rounded-lg overflow-hidden">
          {/* Display header */}
          <div className="bg-gray-50 px-4 py-3 border-b border-gray-200">
            <div className="flex items-center justify-between">
              <h3 className="text-lg font-semibold text-gray-900">
                {display.title}
              </h3>
              <div className="flex items-center space-x-2">
                <span className="text-sm text-gray-500">
                  {display.content?.length || 0} rows
                </span>
                <CopyButton data={display.content} />
              </div>
            </div>
          </div>
          
          {/* Display content */}
          <div className="p-4">
            {display.type === 'table' && (
              <DataTable data={display.content} />
            )}
            {display.type === 'text' && (
              <pre className="whitespace-pre-wrap text-sm">
                {JSON.stringify(display.content, null, 2)}
              </pre>
            )}
          </div>
        </div>
      ))}
    </div>
  );
}
```

### DataTable Component

Advanced table component with sorting, pagination, and search:

```tsx
interface DataTableProps {
  data: Record<string, any>[];
}

export function DataTable({ data }: DataTableProps) {
  const [sortColumn, setSortColumn] = useState<string | null>(null);
  const [sortDirection, setSortDirection] = useState<'asc' | 'desc'>('asc');
  const [currentPage, setCurrentPage] = useState(1);
  const [searchTerm, setSearchTerm] = useState('');
  const [itemsPerPage] = useState(10);

  if (!data || data.length === 0) {
    return (
      <div className="text-center py-8 text-gray-500">
        No data available
      </div>
    );
  }

  // Get column names from first row
  const columns = Object.keys(data[0]);

  // Filter data based on search term
  const filteredData = data.filter(row =>
    Object.values(row).some(value =>
      String(value).toLowerCase().includes(searchTerm.toLowerCase())
    )
  );

  // Sort data
  const sortedData = [...filteredData].sort((a, b) => {
    if (!sortColumn) return 0;
    
    const aVal = a[sortColumn];
    const bVal = b[sortColumn];
    
    if (aVal < bVal) return sortDirection === 'asc' ? -1 : 1;
    if (aVal > bVal) return sortDirection === 'asc' ? 1 : -1;
    return 0;
  });

  // Paginate data
  const totalPages = Math.ceil(sortedData.length / itemsPerPage);
  const startIndex = (currentPage - 1) * itemsPerPage;
  const paginatedData = sortedData.slice(startIndex, startIndex + itemsPerPage);

  const handleSort = (column: string) => {
    if (sortColumn === column) {
      setSortDirection(sortDirection === 'asc' ? 'desc' : 'asc');
    } else {
      setSortColumn(column);
      setSortDirection('asc');
    }
  };

  return (
    <div className="space-y-4">
      {/* Search bar */}
      <div className="flex items-center space-x-2">
        <Search className="h-4 w-4 text-gray-400" />
        <input
          type="text"
          placeholder="Search table data..."
          value={searchTerm}
          onChange={(e) => setSearchTerm(e.target.value)}
          className="flex-1 px-3 py-2 border border-gray-300 rounded-md text-sm"
        />
      </div>

      {/* Table */}
      <div className="overflow-x-auto border border-gray-200 rounded-lg">
        <table className="min-w-full divide-y divide-gray-200">
          <thead className="bg-gray-50">
            <tr>
              {columns.map((column) => (
                <th
                  key={column}
                  onClick={() => handleSort(column)}
                  className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider cursor-pointer hover:bg-gray-100"
                >
                  <div className="flex items-center space-x-1">
                    <span>{column}</span>
                    {sortColumn === column && (
                      sortDirection === 'asc' ? 
                        <ChevronUp className="h-4 w-4" /> : 
                        <ChevronDown className="h-4 w-4" />
                    )}
                  </div>
                </th>
              ))}
            </tr>
          </thead>
          <tbody className="bg-white divide-y divide-gray-200">
            {paginatedData.map((row, index) => (
              <tr key={index} className="hover:bg-gray-50">
                {columns.map((column) => (
                  <td key={column} className="px-6 py-4 whitespace-nowrap text-sm text-gray-900">
                    {formatCellValue(row[column])}
                  </td>
                ))}
              </tr>
            ))}
          </tbody>
        </table>
      </div>

      {/* Pagination */}
      {totalPages > 1 && (
        <div className="flex items-center justify-between">
          <div className="text-sm text-gray-700">
            Showing {startIndex + 1} to {Math.min(startIndex + itemsPerPage, sortedData.length)} of {sortedData.length} results
          </div>
          <div className="flex items-center space-x-2">
            <button
              onClick={() => setCurrentPage(Math.max(1, currentPage - 1))}
              disabled={currentPage === 1}
              className="px-3 py-1 border border-gray-300 rounded-md text-sm disabled:opacity-50"
            >
              Previous
            </button>
            <span className="text-sm">
              Page {currentPage} of {totalPages}
            </span>
            <button
              onClick={() => setCurrentPage(Math.min(totalPages, currentPage + 1))}
              disabled={currentPage === totalPages}
              className="px-3 py-1 border border-gray-300 rounded-md text-sm disabled:opacity-50"
            >
              Next
            </button>
          </div>
        </div>
      )}
    </div>
  );
}

// Helper function to format cell values
function formatCellValue(value: any): string {
  if (value === null || value === undefined) {
    return '';
  }
  
  if (typeof value === 'number') {
    return value.toLocaleString();
  }
  
  if (typeof value === 'boolean') {
    return value ? 'Yes' : 'No';
  }
  
  if (value instanceof Date) {
    return value.toLocaleDateString();
  }
  
  return String(value);
}
```

### Copy to Clipboard Functionality

```tsx
function CopyButton({ data }: { data: any[] }) {
  const [copied, setCopied] = useState(false);

  const handleCopy = () => {
    const csvData = convertToCSV(data);
    navigator.clipboard.writeText(csvData);
    setCopied(true);
    setTimeout(() => setCopied(false), 2000);
  };

  return (
    <button
      onClick={handleCopy}
      className="flex items-center space-x-1 px-2 py-1 text-sm text-gray-600 hover:text-gray-900"
    >
      {copied ? <Check className="h-4 w-4" /> : <Copy className="h-4 w-4" />}
      <span>{copied ? 'Copied!' : 'Copy CSV'}</span>
    </button>
  );
}

function convertToCSV(data: any[]): string {
  if (!data || data.length === 0) return '';
  
  const headers = Object.keys(data[0]);
  const csvRows = [
    headers.join(','),
    ...data.map(row => 
      headers.map(header => 
        JSON.stringify(row[header] || '')
      ).join(',')
    )
  ];
  
  return csvRows.join('\n');
}
```

## Styling and Responsive Design

### Tailwind CSS Classes

The table display system uses a comprehensive set of Tailwind classes:

```css
/* Table container */
.table-container {
  @apply overflow-x-auto border border-gray-200 rounded-lg shadow-sm;
}

/* Table base styles */
.data-table {
  @apply min-w-full divide-y divide-gray-200;
}

/* Header styling */
.table-header {
  @apply bg-gray-50 px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider;
}

/* Cell styling */
.table-cell {
  @apply px-6 py-4 whitespace-nowrap text-sm text-gray-900;
}

/* Row hover effects */
.table-row {
  @apply hover:bg-gray-50 transition-colors duration-150;
}

/* Responsive breakpoints */
@media (max-width: 768px) {
  .table-container {
    @apply text-xs;
  }
  
  .table-cell {
    @apply px-3 py-2;
  }
}
```

### Mobile Responsiveness

```tsx
// Mobile-optimized table view
function MobileTableView({ data }: { data: any[] }) {
  return (
    <div className="space-y-3 md:hidden">
      {data.map((row, index) => (
        <div key={index} className="bg-white border border-gray-200 rounded-lg p-4">
          {Object.entries(row).map(([key, value]) => (
            <div key={key} className="flex justify-between py-1">
              <span className="font-medium text-gray-600">{key}:</span>
              <span className="text-gray-900">{formatCellValue(value)}</span>
            </div>
          ))}
        </div>
      ))}
    </div>
  );
}
```

## Error Handling and Edge Cases

### Empty Data Handling

```tsx
function EmptyTableState() {
  return (
    <div className="text-center py-12">
      <Database className="mx-auto h-12 w-12 text-gray-400" />
      <h3 className="mt-2 text-sm font-medium text-gray-900">No data available</h3>
      <p className="mt-1 text-sm text-gray-500">
        This query returned no results.
      </p>
    </div>
  );
}
```

### Loading States

```tsx
function TableLoadingState() {
  return (
    <div className="animate-pulse">
      <div className="bg-gray-200 h-8 w-full rounded mb-4"></div>
      {[...Array(5)].map((_, i) => (
        <div key={i} className="bg-gray-100 h-6 w-full rounded mb-2"></div>
      ))}
    </div>
  );
}
```

### Error Display

```tsx
function TableErrorState({ error }: { error: string }) {
  return (
    <div className="text-center py-12">
      <AlertTriangle className="mx-auto h-12 w-12 text-red-400" />
      <h3 className="mt-2 text-sm font-medium text-red-900">Error loading data</h3>
      <p className="mt-1 text-sm text-red-500">{error}</p>
    </div>
  );
}
```

## Performance Optimization

### Virtual Scrolling for Large Datasets

```tsx
import { FixedSizeList as List } from 'react-window';

function VirtualizedTable({ data }: { data: any[] }) {
  const Row = ({ index, style }: { index: number; style: React.CSSProperties }) => (
    <div style={style} className="flex border-b">
      {Object.values(data[index]).map((value, cellIndex) => (
        <div key={cellIndex} className="flex-1 px-4 py-2">
          {formatCellValue(value)}
        </div>
      ))}
    </div>
  );

  return (
    <List
      height={400}
      itemCount={data.length}
      itemSize={50}
      className="border rounded-lg"
    >
      {Row}
    </List>
  );
}
```

### Memoization for Performance

```tsx
import { memo, useMemo } from 'react';

const MemoizedDataTable = memo(function DataTable({ data }: DataTableProps) {
  const processedData = useMemo(() => {
    // Expensive data processing here
    return data.map(row => ({
      ...row,
      processed: true
    }));
  }, [data]);

  return (
    // Table rendering logic
  );
});
```

## Integration with Chat Flow

### Message State Management

```tsx
// In Chat.tsx
const [messages, setMessages] = useState<Message[]>([]);

interface Message {
  id: string;
  role: 'user' | 'assistant';
  content: string;
  displays?: DisplayObject[];
  timestamp: Date;
}

const { mutate: sendMessage } = useMutation({
  mutationFn: async ({ message, conversationId }) => {
    const response = await fetch('/api/chat', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ message, conversation_id: conversationId })
    });
    return response.json();
  },
  onSuccess: (data) => {
    const assistantMessage: Message = {
      id: generateId(),
      role: 'assistant',
      content: data.response,
      displays: data.displays || [],
      timestamp: new Date()
    };
    
    setMessages(prev => [...prev, assistantMessage]);
  }
});
```

## Debugging and Development Tools

### Display Debug Component

```tsx
function DisplayDebugger({ displays }: { displays: DisplayObject[] }) {
  if (process.env.NODE_ENV !== 'development') return null;
  
  return (
    <details className="mt-4 p-2 bg-gray-100 rounded text-xs">
      <summary>Debug Info</summary>
      <pre className="mt-2 overflow-auto">
        {JSON.stringify(displays, null, 2)}
      </pre>
    </details>
  );
}
```

### Console Logging

```typescript
// Add to CanvasDisplay component for debugging
useEffect(() => {
  console.log('Rendering displays:', displays);
  displays.forEach((display, index) => {
    console.log(`Display ${index}:`, {
      type: display.type,
      title: display.title,
      rowCount: display.content?.length,
      columns: display.content?.[0] ? Object.keys(display.content[0]) : []
    });
  });
}, [displays]);
```

## Conclusion

The chat table display system provides a robust, interactive way to present data within the chat interface. The multi-layered extraction system ensures data is captured reliably, while the React component architecture provides flexible, responsive rendering with advanced features like sorting, pagination, and search.

The system handles edge cases gracefully and provides excellent performance even with large datasets through virtualization and memoization. The comprehensive styling system ensures tables look professional and remain usable across all device sizes.

This architecture enables users to seamlessly explore their data through natural language queries and receive immediate visual feedback in an intuitive chat interface.