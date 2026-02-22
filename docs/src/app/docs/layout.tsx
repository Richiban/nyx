import DocsSidebar from "@/components/docs/DocsSidebar";
import DocsTopNav from "@/components/docs/DocsTopNav";

export default function DocsLayout({
  children,
}: {
  children: React.ReactNode;
}) {
  return (
    <div className="min-h-screen px-6 pb-16 pt-8">
      <div className="mx-auto flex flex-col gap-6">
        <DocsTopNav />
        <div className="grid gap-6 lg:grid-cols-[260px_1fr]">
          <DocsSidebar />
          <main className="nyx-surface p-8 md:p-10">
            {children}
          </main>
        </div>
      </div>
    </div>
  );
}
