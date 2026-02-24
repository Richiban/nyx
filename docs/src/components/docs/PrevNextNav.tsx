import { Link } from "react-router-dom";
import { ChevronLeft, ChevronRight } from "lucide-react";
import type { DocPage } from "@/content/getting-started";
import type { NavSection } from "@/content/navigation";

interface PrevNextNavProps {
  prev: { path: string; page: DocPage; section: NavSection } | null;
  next: { path: string; page: DocPage; section: NavSection } | null;
}

export function PrevNextNav({ prev, next }: PrevNextNavProps) {
  return (
    <div className="flex items-stretch gap-4 mt-12 pt-8 border-t border-border">
      {prev ? (
        <Link
          to={prev.path}
          className="flex-1 group flex items-center gap-3 p-4 rounded-lg border border-border hover:border-primary/30 hover:bg-accent/50 transition-colors"
        >
          <ChevronLeft className="h-4 w-4 text-muted-foreground group-hover:text-primary transition-colors" />
          <div className="text-right flex-1">
            <div className="text-xs text-muted-foreground">{prev.section.title}</div>
            <div className="text-sm font-medium group-hover:text-primary transition-colors">{prev.page.title}</div>
          </div>
        </Link>
      ) : (
        <div className="flex-1" />
      )}
      {next ? (
        <Link
          to={next.path}
          className="flex-1 group flex items-center gap-3 p-4 rounded-lg border border-border hover:border-primary/30 hover:bg-accent/50 transition-colors"
        >
          <div>
            <div className="text-xs text-muted-foreground">{next.section.title}</div>
            <div className="text-sm font-medium group-hover:text-primary transition-colors">{next.page.title}</div>
          </div>
          <ChevronRight className="h-4 w-4 ml-auto text-muted-foreground group-hover:text-primary transition-colors" />
        </Link>
      ) : (
        <div className="flex-1" />
      )}
    </div>
  );
}
