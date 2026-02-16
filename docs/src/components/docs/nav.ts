export type DocNavItem = {
  title: string;
  href: string;
};

export type DocNavGroup = {
  title: string;
  items: DocNavItem[];
};

export const docNav: DocNavGroup[] = [
  {
    title: "Get started",
    items: [
      { title: "Overview", href: "/docs/overview" },
      { title: "Getting started", href: "/docs/getting-started" },
      { title: "Syntax", href: "/docs/syntax" },
    ],
  },
  {
    title: "Core concepts",
    items: [
      { title: "Types", href: "/docs/types" },
      { title: "Functions & Lambdas", href: "/docs/functions" },
      { title: "Records & Tuples", href: "/docs/records" },
      { title: "Collections", href: "/docs/collections" },
      { title: "Pattern matching", href: "/docs/pattern-matching" },
      { title: "Pipelines", href: "/docs/pipelines" },
      { title: "Operators", href: "/docs/operators" },
    ],
  },
  {
    title: "Advanced topics",
    items: [
      { title: "Contexts", href: "/docs/contexts" },
      { title: "Error handling", href: "/docs/errors" },
      { title: "Workflows", href: "/docs/workflows" },
      { title: "Modules", href: "/docs/modules" },
    ],
  },
  {
    title: "Reference",
    items: [
      { title: "Design principles", href: "/docs/principles" },
    ],
  },
];
