# Kultivate AI - System Architecture & Configuration

## Overview

Kultivate AI is a full-stack chat application that combines React frontend with Python Flask backend, providing AI-powered data insights through integration with Google Gemini, Keboola Cloud, and BigQuery. The application serves as an intelligent assistant for data visualization, code generation, and API integration.

## System Architecture

### Frontend Architecture
- **Framework**: React 18 with TypeScript
- **Styling**: Tailwind CSS with shadcn/ui component library
- **Routing**: Wouter for client-side routing
- **State Management**: TanStack Query for server state management
- **Build Tool**: Vite for development and production builds
- **UI Components**: Radix UI primitives with custom theming

### Backend Architecture
- **Primary Server**: Python Flask (port 8081) for AI processing and API integrations
- **Proxy Server**: Node.js Express (port 5000) for authentication and static file serving
- **AI Integration**: Google Gemini 2.0 Flash for natural language processing
- **Data Sources**: Keboola Cloud API and Google BigQuery
- **Session Management**: PostgreSQL-backed session storage

### Database Layer
- **Primary Database**: PostgreSQL (configured via Drizzle ORM)
- **Schema Management**: Drizzle Kit for migrations and schema management
- **Tables**: Users, sessions, conversations, messages
- **Session Store**: connect-pg-simple for Express session persistence

## Key Components

### Authentication System
- **Strategy**: Passport.js with local strategy
- **Password Security**: Scrypt-based hashing with salt
- **Session Management**: Express sessions with PostgreSQL store
- **Protection**: Protected routes with authentication middleware

### AI Processing Pipeline
- **Primary AI**: Google Gemini 2.0 Flash with function calling
- **Tools Available**: SQL query execution, Keboola bucket listing, table details, location services
- **Response Format**: Structured JSON with content and display components
- **Error Handling**: Comprehensive logging and fallback mechanisms

### Data Integration Layer
- **Keboola Storage**: API client for data bucket and table operations
- **BigQuery**: Google Cloud client for SQL query execution
- **Credentials**: Service account JSON for Google Cloud services
- **Environment Variables**: Secure configuration for API keys and tokens

### Frontend Components
- **Chat Interface**: Real-time messaging with typing indicators
- **Canvas Display**: Multi-format content rendering (text, code, tables, visualizations)
- **Sidebar Navigation**: Conversation management and history
- **Authentication UI**: Login/register forms with validation

## Data Flow

1. **User Input**: Message submitted through chat interface
2. **Authentication**: Express middleware validates user session
3. **Message Processing**: Content forwarded to Flask backend via internal API
4. **AI Processing**: Gemini processes message with available tools and context
5. **Data Retrieval**: Tools execute against Keboola/BigQuery as needed
6. **Response Generation**: AI formats response with appropriate displays
7. **Database Storage**: Conversation and messages persisted to PostgreSQL
8. **UI Update**: Frontend receives structured response and renders components

## External Dependencies

### Required Environment Variables
- `DATABASE_URL`: PostgreSQL connection string
- `KBC_API_URL`: Keboola Cloud API endpoint
- `KBC_STORAGE_TOKEN`: Keboola authentication token
- `KBC_WORKSPACE_SCHEMA`: BigQuery schema identifier
- `GOOGLE_APPLICATION_CREDENTIALS`: Path to service account JSON
- `GEMINI_API_KEY`: Google AI API key
- `SESSION_SECRET`: Express session encryption key

### Third-Party Services
- **Google Cloud Platform**: BigQuery data warehouse and AI services
- **Keboola Cloud**: Data platform for ETL and storage
- **Neon Database**: Serverless PostgreSQL hosting
- **Replit Infrastructure**: Development and deployment environment

### Key Libraries
- **Frontend**: React, Wouter, TanStack Query, Tailwind CSS, Radix UI
- **Backend**: Flask, Google Generative AI SDK, Keboola API client
- **Database**: Drizzle ORM, pg (PostgreSQL driver)
- **Authentication**: Passport.js, express-session, connect-pg-simple

## Deployment Strategy

### Development Environment
- **Command**: `npm run dev`
- **Frontend**: Vite dev server with HMR
- **Backend**: Direct Python execution with auto-reload
- **Database**: Local PostgreSQL or Neon connection

### Production Environment
- **Build Process**: `npm run build` compiles React and Node.js server
- **Startup**: Custom deployment script manages both servers
- **Port Configuration**: 
  - Frontend/Proxy: 5000 (external: 80)
  - Backend API: 8081 (internal only)
- **Process Management**: Concurrent server execution with health checks
- **Static Assets**: Express serves compiled React build

### Replit Configuration
- **Modules**: Node.js 20, PostgreSQL 16, Python 3.11, Web
- **Deployment Target**: Autoscale with build command
- **Port Mapping**: External port 80 maps to internal port 5000
- **Environment**: Production variables configured in Replit secrets

## Recent Changes

- **June 23, 2025**: COMPLETE FIX - Table display issue fully resolved
  - Fixed faulty check logic in `backend/main_2.py` that was preventing proper table rendering
  - Removed misleading `elif query_data:` condition that caused confusing error logs
  - Implemented proper `isinstance(query_data, list)` check as per user-provided fix instructions
  - Added dedicated message handler in `server/index.ts` to properly process displays from backend
  - Fixed React Query mutation in `client/src/components/chat.tsx` to use correct `/api/chat` endpoint
  - CRITICAL FIX: Updated ChatBubble component to properly handle displays array structure
  - Fixed displays property handling in `client/src/components/ui/chat-bubble.tsx` to support both singular and plural display formats
  - Fixed backend TypeError in `backend/main_2.py` by adding null check for message_content.parts to prevent crashes
  - Added missing `re` module import to fix UnboundLocalError in fallback display generation
  - CanvasDisplay component completely rewritten with clean, functional table rendering
  - Backend successfully processes and returns 64 Kapwa Gardens table names from BigQuery
  - Complete data flow verified: BigQuery → Backend → Node.js Proxy → Frontend → User display
  - Table displays with proper styling, scrollable content, and copy functionality
  - System now correctly handles all 64 Kapwa Gardens tables including Close-Out Sales, vendor data, market recaps, and event information
  - Frontend debugging added to React Query mutation for response validation

- **June 23, 2025**: Fixed critical frontend-backend API communication and data access
  - Implemented manual proxy middleware between Node.js (port 5000) and Flask (port 8081)
  - Fixed "New Chat" button functionality - now creates and opens conversations properly
  - Added missing `/api/conversations/{id}` route for individual conversation access
  - Fixed BigQuery authentication issue by correcting missing `re` module import
  - Improved Flask server readiness detection to prevent connection errors
  - Complete API endpoints now responding correctly (HTTP 200 status)
  - Chat functionality fully operational - users can create conversations and send messages
  - Data analysis pipeline working - AI successfully retrieving and analyzing real user data
  - Verified data access: AI can query BigQuery tables and Keboola buckets successfully

## Changelog

- June 23, 2025. Initial setup and frontend-backend integration completed

## User Preferences

Preferred communication style: Simple, everyday language.