import { useAuth } from "@/hooks/use-auth";
import { Button } from "@/components/ui/button";
import { LogOut } from "lucide-react";

export function LogoutButton() {
  const { logoutMutation } = useAuth();

  return (
    <Button
      variant="ghost"
      size="sm"
      onClick={() => logoutMutation.mutate()}
      disabled={logoutMutation.isPending}
      className="flex items-center gap-2 text-neutral-600 hover:text-neutral-800"
    >
      <LogOut className="w-4 h-4" />
      Logout
    </Button>
  );
}