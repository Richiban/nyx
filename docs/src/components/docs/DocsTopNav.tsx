import Link from "next/link";

export default function DocsTopNav() {
  return (
    <div className="nyx-surface flex flex-wrap items-center gap-4 px-5 py-4">
      <div className="flex items-center gap-3">
        <div className="flex h-9 w-9 items-center justify-center rounded-xl bg-[color:var(--nyx-accent)] text-sm font-semibold text-white">
          N
        </div>
        <div>
          <p className="text-xs uppercase tracking-[0.2em] text-[color:var(--nyx-muted)]">
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
