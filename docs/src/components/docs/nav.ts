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
      { title: "Pattern matching", href: "/docs/pattern-matching" },
      { title: "Pipelines", href: "/docs/pipelines" },
    ],
  },
  {
    title: "Workflows",
    items: [
      { title: "Contexts", href: "/docs/contexts" },
      { title: "Error handling", href: "/docs/errors" },
    ],
  },
];
