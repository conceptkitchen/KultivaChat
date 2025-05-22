import { GoogleGenerativeAI } from "@google/generative-ai";

// Function to get API key from environment variables
const getApiKey = (): string => {
  const apiKey = process.env.GEMINI_API_KEY;
  if (!apiKey) {
    throw new Error("GEMINI_API_KEY is not set in environment variables");
  }
  return apiKey;
};

// Initialize the Google Generative AI client
const initializeGeminiClient = () => {
  try {
    const apiKey = getApiKey();
    console.log("Initializing Gemini client with API key (first few chars):", apiKey.substring(0, 5) + "...");
    return new GoogleGenerativeAI(apiKey);
  } catch (error) {
    console.error("Failed to initialize Gemini client:", error);
    throw error;
  }
};

// Generate a response using Gemini
export async function generateGeminiResponse(
  prompt: string,
  conversationHistory: Array<{ role: string; content: string }> = []
): Promise<{ content: string; displays?: any[] }> {
  try {
    const genAI = initializeGeminiClient();
    const model = genAI.getGenerativeModel({ model: "gemini-2.0-flash" });

    // Format conversation history for the model
    const formattedHistory = conversationHistory.map(message => ({
      role: message.role === "user" ? "user" : "model",
      parts: [{ text: message.content }]
    }));

    // Start a chat session
    const chat = model.startChat({
      history: formattedHistory,
      generationConfig: {
        temperature: 0.7,
        topP: 0.8,
        topK: 40,
        maxOutputTokens: 1024,
      }
    });

    // Generate response
    const result = await chat.sendMessage(prompt);
    const response = result.response;
    const text = response.text();

    // Extract potential code or visualization data from response
    let displays = [];
    
    // Check for code blocks
    const codeBlockRegex = /```([a-zA-Z]*)\n([\s\S]*?)```/g;
    let codeMatch;
    
    while ((codeMatch = codeBlockRegex.exec(text)) !== null) {
      const language = codeMatch[1] || "text";
      const code = codeMatch[2].trim();
      
      displays.push({
        type: "code",
        language: language,
        content: code
      });
    }

    // Clean text by removing code blocks for the final response
    let cleanedText = text.replace(codeBlockRegex, "").trim();
    
    if (cleanedText === "") {
      cleanedText = "Here's what I've prepared for you:";
    }

    return {
      content: cleanedText,
      displays: displays.length > 0 ? displays : undefined
    };
  } catch (error) {
    console.error("Error generating Gemini response:", error);
    return {
      content: "I'm sorry, but I encountered an issue generating a response. Please try again later."
    };
  }
}