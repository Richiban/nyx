import { useEffect, useState } from "react";
import { useNavigate } from "react-router-dom";
import { getAllPages } from "@/content/navigation";
import {
  CommandDialog,
  CommandInput,
  CommandList,
  CommandEmpty,
  CommandGroup,
  CommandItem,
} from "@/components/ui/command";
import { FileText } from "lucide-react";

interface SearchDialogProps {
  open: boolean;
  onOpenChange: (open: boolean) => void;
}

export function SearchDialog({ open, onOpenChange }: SearchDialogProps) {
  const navigate = useNavigate();
  const allPages = getAllPages();

  useEffect(() => {
    const down = (e: KeyboardEvent) => {
      if (e.key === "k" && (e.metaKey || e.ctrlKey)) {
        e.preventDefault();
        onOpenChange(!open);
      }
    };
    document.addEventListener("keydown", down);
    return () => document.removeEventListener("keydown", down);
  }, [open, onOpenChange]);

  const handleSelect = (path: string) => {
    navigate(path);
    onOpenChange(false);
  };

  // Group pages by section
  const grouped = new Map<string, typeof allPages>();
  allPages.forEach((entry) => {
    const key = entry.section.title;
    if (!grouped.has(key)) grouped.set(key, []);
    grouped.get(key)!.push(entry);
  });

  return (
    <CommandDialog open={open} onOpenChange={onOpenChange}>
      <CommandInput placeholder="Search documentation..." />
      <CommandList>
        <CommandEmpty>No results found.</CommandEmpty>
        {Array.from(grouped.entries()).map(([sectionTitle, pages]) => (
          <CommandGroup key={sectionTitle} heading={sectionTitle}>
            {pages.map((entry) => (
              <CommandItem
                key={entry.path}
                value={`${entry.section.title} ${entry.page.title} ${entry.page.description || ""}`}
                onSelect={() => handleSelect(entry.path)}
              >
                <FileText className="mr-2 h-4 w-4 text-muted-foreground" />
                <div>
                  <div className="text-sm font-medium">{entry.page.title}</div>
                  {entry.page.description && (
                    <div className="text-xs text-muted-foreground">{entry.page.description}</div>
                  )}
                </div>
              </CommandItem>
            ))}
          </CommandGroup>
        ))}
      </CommandList>
    </CommandDialog>
  );
}
