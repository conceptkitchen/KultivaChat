import React, { useState } from "react";
import { Button } from "@/components/ui/button";
import { Textarea } from "@/components/ui/textarea";
import { Card, CardContent, CardFooter, CardHeader, CardTitle } from "@/components/ui/card";
import { Loader2 } from "lucide-react";
import { CanvasDisplay } from "@/components/ui/canvas-display";

export function GeminiTester() {
  const [prompt, setPrompt] = useState("");
  const [response, setResponse] = useState("");
  const [displays, setDisplays] = useState([]);
  const [isLoading, setIsLoading] = useState(false);
  const [error, setError] = useState("");

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    
    if (!prompt.trim()) return;
    
    setIsLoading(true);
    setError("");
    
    try {
      const result = await fetch("/api/gemini-test", {
        method: "POST",
        headers: {
          "Content-Type": "application/json",
        },
        body: JSON.stringify({
          conversationId: "test",
          content: prompt
        }),
      });
      
      if (!result.ok) {
        throw new Error(`Error: ${result.status} - ${await result.text()}`);
      }
      
      const data = await result.json();
      
      if (data.assistantMessage) {
        setResponse(data.assistantMessage.content);
        setDisplays(data.assistantMessage.displays || []);
      } else {
        throw new Error("Invalid response format");
      }
    } catch (err) {
      console.error("Error testing Gemini:", err);
      setError(err.message || "Failed to get response from Gemini");
    } finally {
      setIsLoading(false);
    }
  };

  return (
    <div className="w-full max-w-3xl mx-auto mt-10 px-4">
      <Card className="bg-white shadow-md">
        <CardHeader>
          <CardTitle className="text-center text-primary">
            Gemini 2.0 Flash Test
          </CardTitle>
        </CardHeader>
        <CardContent>
          <form onSubmit={handleSubmit}>
            <div className="space-y-4">
              <div className="space-y-2">
                <label htmlFor="prompt" className="text-sm font-medium">
                  Enter your prompt:
                </label>
                <Textarea
                  id="prompt"
                  placeholder="What would you like to ask Gemini?"
                  value={prompt}
                  onChange={(e) => setPrompt(e.target.value)}
                  className="min-h-[100px]"
                />
              </div>

              <Button
                type="submit"
                className="w-full"
                disabled={isLoading || !prompt.trim()}
              >
                {isLoading ? (
                  <>
                    <Loader2 className="mr-2 h-4 w-4 animate-spin" />
                    Getting response...
                  </>
                ) : (
                  "Get AI Response"
                )}
              </Button>
            </div>
          </form>

          {error && (
            <div className="mt-4 p-3 bg-red-50 border border-red-200 rounded-md text-red-600 text-sm">
              {error}
            </div>
          )}

          {response && (
            <div className="mt-6 space-y-4">
              <h3 className="font-medium text-lg">Gemini Response:</h3>
              <div className="p-4 bg-primary-light/10 rounded-xl whitespace-pre-wrap">
                {response}
              </div>
              
              {displays && displays.length > 0 && (
                <div className="mt-4">
                  <h4 className="font-medium mb-2">Generated Content:</h4>
                  <CanvasDisplay displays={displays} />
                </div>
              )}
            </div>
          )}
        </CardContent>
        <CardFooter className="justify-between border-t pt-4 text-xs text-neutral-500">
          <div>Using Google Gemini 2.0 Flash model</div>
        </CardFooter>
      </Card>
    </div>
  );
}