import Link from "next/link";
import NanyxLogo from "@/components/NanyxLogo";

export default function DocsTopNav() {
  return (
    <div className="nyx-surface flex flex-wrap items-center gap-4 px-5 py-4">
      <div className="flex items-center gap-3">
        <div className="flex h-32 w-32 items-center justify-center rounded-xl">
          <NanyxLogo height="100%" />
        </div>
        <div>
          <p
            className="text-xs uppercase tracking-[0.2em] text-[color:var(--nyx-muted)]"
            style={{ fontFamily: "'Audiowide', sans-serif" }}
          >
            Nanyx
          </p>
          <p className="text-base font-semibold">Documentation</p>
        </div>
      </div>
      <div className="flex-1" />
      <div className="flex flex-1 items-center gap-3 md:flex-none">
        <input
          placeholder="Search docs"
          className="w-full rounded-full border border-[color:var(--nyx-border)] bg-white/70 px-4 py-2 text-sm focus:outline-none focus:ring-2 focus:ring-[color:var(--nyx-accent)] md:w-64"
        />
        <Link
          href="/"
          className="rounded-full border border-[color:var(--nyx-border)] bg-white/70 px-4 py-2 text-sm font-semibold"
        >
          Home
        </Link>
      </div>
    </div>
  );
}
