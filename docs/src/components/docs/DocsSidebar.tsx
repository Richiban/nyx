"use client";

import Link from "next/link";
import { usePathname } from "next/navigation";
import { docNav } from "./nav";

export default function DocsSidebar({
  isOpen,
  onClose,
}: {
  isOpen?: boolean;
  onClose?: () => void;
}) {
  const pathname = usePathname();

  return (
    <aside
      className={`nyx-surface overflow-y-auto p-6
        fixed inset-y-0 left-0 z-50 w-72 rounded-none transition-transform duration-300
        ${isOpen ? "translate-x-0" : "-translate-x-full"}
        lg:static lg:block lg:h-fit lg:w-full lg:translate-x-0 lg:rounded-3xl lg:transition-none`}
    >
      <div className="mb-6 flex items-center justify-between">
        <div>
          <p className="text-xs uppercase tracking-[0.2em] text-[color:var(--nyx-muted)]">
            Navigate
          </p>
          <h2 className="text-lg font-semibold">Nyx docs</h2>
        </div>
        <button
          onClick={onClose}
          className="flex h-8 w-8 items-center justify-center rounded-lg hover:bg-black/5 lg:hidden"
          aria-label="Close menu"
        >
          <svg
            xmlns="http://www.w3.org/2000/svg"
            width="16"
            height="16"
            viewBox="0 0 24 24"
            fill="none"
            stroke="currentColor"
            strokeWidth="2"
            strokeLinecap="round"
            strokeLinejoin="round"
          >
            <line x1="18" y1="6" x2="6" y2="18" />
            <line x1="6" y1="6" x2="18" y2="18" />
          </svg>
        </button>
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
                    onClick={onClose}
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
