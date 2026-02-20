import DocsLayoutClient from "@/components/docs/DocsLayoutClient";

export default function DocsLayout({
  children,
}: {
  children: React.ReactNode;
}) {
  return <DocsLayoutClient>{children}</DocsLayoutClient>;
}
