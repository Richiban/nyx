import Link from "next/link";

export default function Home() {
  return (
    <div className="min-h-screen">
      <header className="nyx-grid">
        <div className="mx-auto flex items-center justify-between px-6 py-6">
          <div className="flex items-center gap-3">
            <div className="nyx-surface flex h-10 w-10 items-center justify-center rounded-xl text-lg font-semibold">
              N
            </div>
            <div>
              <p className="text-sm uppercase tracking-[0.2em] text-[color:var(--nyx-muted)]">
                Nanyx Language
              </p>
              <p className="text-xl font-semibold">Documentation</p>
            </div>
          </div>
          <div className="hidden items-center gap-3 md:flex">
            <span className="nyx-pill rounded-full px-4 py-2 text-xs uppercase tracking-[0.2em]">
              v0.x
            </span>
            <Link
              href="/docs"
              className="rounded-full bg-[color:var(--nyx-accent)] px-5 py-2 text-sm font-semibold text-white shadow"
            >
              Read the docs
            </Link>
          </div>
        </div>
      </header>

      <main className="mx-auto grid gap-10 px-6 pb-24 pt-12 md:grid-cols-[1.2fr_0.8fr]">
        <section className="nyx-fade-in space-y-8">
          <div className="nyx-pill inline-flex items-center gap-2 rounded-full px-4 py-2 text-xs uppercase tracking-[0.2em]">
            <span className="h-2 w-2 rounded-full bg-[var(--nyx-accent)]" />
            a language for expressive workflows
          </div>
          <h1 className="text-4xl font-semibold leading-tight md:text-6xl">
            Nanyx is a language for composable tools, strong types, and readable
            pipelines.
          </h1>
          <p className="max-w-xl text-lg text-[color:var(--nyx-muted)]">
            The docs are structured like a studio: learn the core ideas, explore
            patterns, and keep the spec close. Built for clarity, not clutter.
          </p>
          <div className="flex flex-wrap gap-4">
            <Link
              href="/docs"
              className="rounded-full bg-[color:var(--nyx-accent)] px-6 py-3 text-sm font-semibold text-white shadow"
            >
              Start here
            </Link>
            <Link
              href="/docs/getting-started"
              className="rounded-full border border-[var(--nyx-border)] bg-white/70 px-6 py-3 text-sm font-semibold"
            >
              Quick tour
            </Link>
          </div>
        </section>

        <section className="nyx-fade-in flex flex-col gap-6 md:items-end">
          <div className="nyx-surface nyx-glow w-full p-6 md:max-w-sm">
            <h2 className="text-lg font-semibold">Explore the docs</h2>
            <p className="mt-2 text-sm text-[color:var(--nyx-muted)]">
              A Gleam-style flow: overview, syntax, patterns, and recipes.
            </p>
            <div className="mt-4 space-y-3 text-sm">
              <Link href="/docs/overview" className="flex items-center gap-2">
                <span className="h-2 w-2 rounded-full bg-[color:var(--nyx-accent)]" />
                Overview
              </Link>
              <Link href="/docs/syntax" className="flex items-center gap-2">
                <span className="h-2 w-2 rounded-full bg-[color:var(--nyx-accent-2)]" />
                Syntax
              </Link>
              <Link href="/docs/pattern-matching" className="flex items-center gap-2">
                <span className="h-2 w-2 rounded-full bg-[color:var(--nyx-accent)]" />
                Pattern matching
              </Link>
            </div>
          </div>
          <div className="nyx-surface nyx-float w-full p-6 md:max-w-sm">
            <h3 className="text-base font-semibold">Nanyx style guide</h3>
            <p className="mt-2 text-sm text-[color:var(--nyx-muted)]">
              Build consistent docs with curated components, MDX blocks, and a
              clean layout.
            </p>
          </div>
        </section>
      </main>
    </div>
  );
}
