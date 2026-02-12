import type { MDXComponents } from "mdx/types";

export function useMDXComponents(components: MDXComponents): MDXComponents {
  return {
    h1: ({ children }) => (
      <h1 className="text-3xl font-semibold leading-tight md:text-4xl">
        {children}
      </h1>
    ),
    h2: ({ children }) => (
      <h2 className="mt-10 text-2xl font-semibold">{children}</h2>
    ),
    h3: ({ children }) => (
      <h3 className="mt-8 text-xl font-semibold">{children}</h3>
    ),
    p: ({ children }) => (
      <p className="mt-4 text-base leading-7 text-[color:var(--nyx-muted)]">
        {children}
      </p>
    ),
    ul: ({ children }) => (
      <ul className="mt-4 list-disc space-y-2 pl-6 text-[color:var(--nyx-muted)]">
        {children}
      </ul>
    ),
    li: ({ children }) => <li className="text-base">{children}</li>,
    pre: ({ children }) => (
      <pre className="mt-6 overflow-x-auto rounded-2xl bg-[#14171d] p-5 text-sm text-slate-100">
        {children}
      </pre>
    ),
    code: ({ children }) => (
      <code className="rounded bg-black/5 px-2 py-1 text-sm text-[#2b2e36]">
        {children}
      </code>
    ),
    ...components,
  };
}
