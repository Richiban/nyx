import DocsSidebar from "@/components/docs/DocsSidebar";
import DocsPageNav from "@/components/docs/DocsPageNav";
import DocsTopNav from "@/components/docs/DocsTopNav";

export default function DocsLayout({
  children,
}: {
  children: React.ReactNode;
}) {
  return (
    <div className="min-h-screen px-6 pb-12 pt-6">
      <div className="mx-auto flex flex-col gap-4">
        <DocsTopNav />
        <div className="grid gap-4 lg:grid-cols-[260px_1fr]">
          <DocsSidebar />
          <main className="nyx-surface p-6 md:p-8">
            {children}
            <DocsPageNav />
          </main>
        </div>
      </div>
    </div>
  );
}
