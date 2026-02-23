"use client";

import Link from "next/link";
import { usePathname } from "next/navigation";
import { flatDocNav } from "./nav";

const normalizePath = (path: string) =>
  path.length > 1 && path.endsWith("/") ? path.slice(0, -1) : path;

export default function DocsPageNav() {
  const pathname = normalizePath(usePathname());
  const currentIndex = flatDocNav.findIndex(
    (item) => normalizePath(item.href) === pathname,
  );

  if (currentIndex === -1) {
    return null;
  }

  const previous = currentIndex > 0 ? flatDocNav[currentIndex - 1] : null;
  const next =
    currentIndex < flatDocNav.length - 1 ? flatDocNav[currentIndex + 1] : null;

  if (!previous && !next) {
    return null;
  }

  return (
    <nav
      aria-label="Documentation pagination"
      className="mt-12 grid gap-4 border-t border-black/10 pt-6 md:grid-cols-2"
      style={{ fontFamily: "'Audiowide', sans-serif" }}
    >
      {previous ? (
        <Link
          href={previous.href}
          className="block rounded-xl p-4 text-sm transition hover:bg-black/5"
        >
          <p className="text-xs uppercase tracking-[0.2em] text-[color:var(--nyx-muted)]">
            Previous
          </p>
          <p className="mt-1 font-medium">{previous.title}</p>
          <p className="mt-1 text-xs text-[color:var(--nyx-muted)]">←</p>
        </Link>
      ) : (
        <div />
      )}

      {next ? (
        <Link
          href={next.href}
          className="block rounded-xl p-4 text-right text-sm transition hover:bg-black/5"
        >
          <p className="text-xs uppercase tracking-[0.2em] text-[color:var(--nyx-muted)]">
            Next
          </p>
          <p className="mt-1 font-medium">{next.title}</p>
          <p className="mt-1 text-xs text-[color:var(--nyx-muted)]">→</p>
        </Link>
      ) : (
        <div />
      )}
    </nav>
  );
}
