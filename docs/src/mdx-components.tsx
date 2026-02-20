import type { MDXComponents } from "mdx/types";
import { CodeBlock } from "./components/CodeBlock";
import type { ComponentPropsWithoutRef } from "react";

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
    pre: ({ children }) => <>{children}</>,
    code: (props: ComponentPropsWithoutRef<"code">) => <CodeBlock {...props} />,
    ...components,
  };
}
