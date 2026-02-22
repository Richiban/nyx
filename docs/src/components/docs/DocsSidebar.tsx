"use client";

import Link from "next/link";
import { usePathname } from "next/navigation";
import { docNav } from "./nav";

export default function DocsSidebar() {
  const pathname = usePathname();

  return (
    <aside className="nyx-surface h-fit w-full p-6">
      <div className="mb-6">
        <p className="text-xs uppercase tracking-[0.2em] text-[color:var(--nyx-muted)]">
          Navigate
        </p>
        <h2 className="text-lg font-semibold">Nyx docs</h2>
      </div>
      <nav className="space-y-6">
        {docNav.map((group) => (
          <div key={group.title} className="space-y-3">
            <p className="text-xs uppercase tracking-[0.2em] text-[color:var(--nyx-muted)]">
              {group.title}
            </p>
            <div className="space-y-2">
              {group.items.map((item) => {
                const isActive = pathname === item.href;
                return (
                  <Link
                    key={item.href}
                    href={item.href}
                    className={`block rounded-xl px-3 py-2 text-sm transition ${
                      isActive
                        ? "bg-[color:var(--nyx-accent)] text-white"
                        : "text-[color:var(--nyx-ink)] hover:bg-black/5"
                    }`}
                  >
                    {item.title}
                  </Link>
                );
              })}
            </div>
          </div>
        ))}
      </nav>
    </aside>
  );
}
