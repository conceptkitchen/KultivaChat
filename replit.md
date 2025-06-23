# Kultivate AI Assistant

## Overview

Kultivate AI Assistant is a full-stack web application that provides AI-powered data visualization, code generation, and integration with Keboola API. The application features a chat-based interface where users can interact with Google's Gemini 2.0 Flash AI model to analyze data, generate code, and visualize information through various display formats.

## System Architecture

The application follows a hybrid architecture with separate frontend and backend services:

- **Frontend**: React-based SPA built with Vite, using TypeScript and modern UI components
- **Backend**: Python Flask API server that handles AI interactions and data processing
- **Database**: PostgreSQL with Drizzle ORM for data persistence
- **AI Integration**: Google Gemini 2.0 Flash for natural language processing and code generation
- **External APIs**: Keboola API for data warehouse operations, Google BigQuery for data analytics

## Key Components

### Frontend Architecture
- **Framework**: React 18 with TypeScript
- **Build Tool**: Vite for fast development and optimized builds
- **UI Library**: Shadcn/ui components with Radix UI primitives
- **Styling**: Tailwind CSS with custom theming (Kultivate gold/yellow branding)
- **State Management**: TanStack Query for server state management
- **Routing**: Wouter for lightweight client-side routing
- **Authentication**: Session-based authentication with protected routes

### Backend Architecture
- **Framework**: Flask (Python) for API endpoints
- **AI Provider**: Google Gemini 2.0 Flash with function calling capabilities
- **Database**: PostgreSQL with connection pooling
- **External Integrations**: 
  - Keboola Storage API for data warehouse operations
  - Google BigQuery for advanced analytics
  - Google Cloud credentials for service authentication

### Database Schema
- **Users**: Authentication and profile management
- **Sessions**: Secure session storage for authentication
- **Conversations**: Chat conversation persistence
- **Messages**: Individual chat messages with AI-generated content and displays

### AI Integration Features
- **Function Calling**: Gemini model can execute SQL queries, list database tables, and retrieve data
- **Multi-modal Outputs**: Supports text, code, tables, and visualizations
- **Real-time Processing**: Streaming responses for better user experience
- **Tool Integration**: Built-in tools for data analysis and code generation

## Data Flow

1. **User Authentication**: Users authenticate via session-based login system
2. **Chat Interface**: Users submit queries through the chat interface
3. **AI Processing**: Backend processes queries using Gemini 2.0 Flash with function calling
4. **Data Retrieval**: AI can execute SQL queries against BigQuery and Keboola data sources
5. **Response Generation**: AI generates structured responses with various display types
6. **Frontend Rendering**: React components render AI responses with appropriate visualizations
7. **Persistence**: Conversations and messages are stored in PostgreSQL database

## External Dependencies

### Core Dependencies
- **Google Gemini API**: Primary AI model for natural language processing
- **Keboola Storage API**: Data warehouse and ETL operations
- **Google BigQuery**: Advanced data analytics and querying
- **PostgreSQL**: Primary database for application data

### Development Dependencies
- **Replit Environment**: Development and hosting platform
- **Node.js 20**: JavaScript runtime for frontend build process
- **Python 3.11**: Backend runtime environment
- **UV Package Manager**: Python dependency management

### UI/UX Dependencies
- **Radix UI**: Accessible component primitives
- **Tailwind CSS**: Utility-first CSS framework
- **Lucide Icons**: Icon library for consistent visual elements
- **Recharts**: Data visualization components

## Deployment Strategy

The application uses a multi-stage deployment approach:

### Development Mode
- Frontend served via Vite dev server with hot reloading
- Backend runs Flask development server
- Proxy configuration routes API calls from frontend to backend

### Production Mode
- Frontend built as static assets and served by Node.js Express server
- Python backend runs as separate Flask application
- Nginx-style reverse proxy setup for API routing
- Environment-specific configuration management

### Hosting Configuration
- **Frontend Port**: 5000 (mapped to external port 80)
- **Backend Port**: 8081 (internal API server)
- **Database**: PostgreSQL via Replit's managed database service
- **Static Assets**: Served from `/dist` directory after build

### Environment Variables
- `DATABASE_URL`: PostgreSQL connection string
- `GEMINI_API_KEY`: Google Gemini API authentication
- `KBC_API_URL`: Keboola connection endpoint
- `KBC_STORAGE_TOKEN`: Keboola authentication token
- `GOOGLE_APPLICATION_CREDENTIALS`: BigQuery service account credentials
- `KBC_WORKSPACE_SCHEMA`: Keboola workspace schema name

## Changelog

```
Changelog:
- June 23, 2025. Initial setup
- June 23, 2025. Fixed deployment issues:
  - Resolved Python dependency error with Runner initialization
  - Created proper WSGI entry point for Gunicorn
  - Fixed port configuration to bind to port 5000
  - Updated Gunicorn configuration for production deployment
  - Successfully deployed with health check and chat API endpoints working
```

## User Preferences

```
Preferred communication style: Simple, everyday language.
```