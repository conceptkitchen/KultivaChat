import { useAuth } from "@/hooks/use-auth";
import { Button } from "@/components/ui/button";
import { LogOut } from "lucide-react";

export function LogoutButton() {
  const { logoutMutation } = useAuth();

  const handleLogout = async (e: React.MouseEvent) => {
    e.preventDefault();
    e.stopPropagation();
    console.log('Logout button clicked - starting logout process');
    logoutMutation.mutate();
  };

  return (
    <Button
      variant="ghost"
      className="w-full justify-start px-3 py-2 text-neutral-600 hover:bg-neutral-50"
      onClick={handleLogout}
      disabled={logoutMutation.isPending}
      type="button"
    >
      <LogOut className="text-neutral-400 mr-2 h-4 w-4" />
      <span>{logoutMutation.isPending ? "Logging out..." : "Log out"}</span>
    </Button>
  );
}