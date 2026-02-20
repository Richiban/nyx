"use client";

import { useState } from "react";
import DocsSidebar from "./DocsSidebar";
import DocsTopNav from "./DocsTopNav";

export default function DocsLayoutClient({
  children,
}: {
  children: React.ReactNode;
}) {
  const [sidebarOpen, setSidebarOpen] = useState(false);

  return (
    <div className="min-h-screen px-6 pb-16 pt-8">
      <div className="mx-auto flex max-w-6xl flex-col gap-6">
        <DocsTopNav onMenuToggle={() => setSidebarOpen((v) => !v)} />
        {sidebarOpen && (
          <div
            className="fixed inset-0 z-40 bg-black/30 lg:hidden"
            onClick={() => setSidebarOpen(false)}
          />
        )}
        <div className="grid gap-6 lg:grid-cols-[260px_1fr]">
          <DocsSidebar
            isOpen={sidebarOpen}
            onClose={() => setSidebarOpen(false)}
          />
          <main className="nyx-surface min-w-0 rounded-3xl p-8 md:p-10">
            {children}
          </main>
        </div>
      </div>
    </div>
  );
}
