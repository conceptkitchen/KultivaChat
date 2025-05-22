import { QueryClient, QueryFunction } from "@tanstack/react-query";

async function throwIfResNotOk(res: Response) {
  if (!res.ok) {
    try {
      // Only throw for actual errors with meaningful content
      const text = await res.text();
      // Skip empty responses or generic failures
      if (text && text !== '{}' && text !== 'Failed to fetch') {
        throw new Error(`${res.status}: ${text}`);
      }
    } catch (e) {
      // If we can't read the response, just use the status text
      if (res.status !== 200) {
        throw new Error(`${res.status}: ${res.statusText}`);
      }
    }
  }
}

export async function apiRequest(
  method: string,
  url: string,
  data?: unknown | undefined,
): Promise<Response> {
  const res = await fetch(url, {
    method,
    headers: data ? { "Content-Type": "application/json" } : {},
    body: data ? JSON.stringify(data) : undefined,
    credentials: "include",
  });

  await throwIfResNotOk(res);
  return res;
}

type UnauthorizedBehavior = "returnNull" | "throw";
export const getQueryFn: <T>(options: {
  on401: UnauthorizedBehavior;
}) => QueryFunction<T> =
  ({ on401: unauthorizedBehavior }) =>
  async ({ queryKey }) => {
    // Handle array query keys for nested resources
    let url = queryKey[0] as string;
    if (queryKey.length > 1 && queryKey[1]) {
      // If we have a nested resource like /api/conversations/123
      url = `${url}/${queryKey[1]}`;
    }
    
    const res = await fetch(url, {
      credentials: "include",
    });

    if (unauthorizedBehavior === "returnNull" && res.status === 401) {
      return null;
    }

    await throwIfResNotOk(res);
    return await res.json();
  };

export const queryClient = new QueryClient({
  defaultOptions: {
    queries: {
      queryFn: getQueryFn({ on401: "throw" }),
      refetchInterval: false,
      refetchOnWindowFocus: false,
      staleTime: Infinity,
      retry: false,
    },
    mutations: {
      retry: false,
    },
  },
});
