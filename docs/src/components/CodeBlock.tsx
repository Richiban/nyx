import { createHighlighter, type Highlighter } from "shiki";
import type { ReactNode } from "react";
import nyxGrammar from "../../public/grammars/nyx.tmLanguage.json";

// We'll cache the highlighter instance
let highlighterPromise: Promise<Highlighter> | null = null;

async function getHighlighter() {
  if (!highlighterPromise) {
    highlighterPromise = (async () => {
      const highlighter = await createHighlighter({
        themes: ["github-light", "github-dark"],
        langs: ["javascript", "typescript", "bash"],
      });
      
      // Load the Nanyx language grammar
      // Note: Type assertion needed because the TextMate grammar format isn't fully typed in Shiki
      await highlighter.loadLanguage({
        ...nyxGrammar,
        name: "nyx",
        scopeName: "source.nyx",
      } as unknown as Parameters<typeof highlighter.loadLanguage>[0]);
      
      return highlighter;
    })();
  }
  return highlighterPromise;
}

async function highlightCode(code: string, lang: string): Promise<string> {
  const highlighter = await getHighlighter();
  return highlighter.codeToHtml(code, {
    lang: lang,
    themes: {
      light: "github-light",
      dark: "github-dark",
    },
  });
}

export async function CodeBlock({
  children,
  className,
}: {
  children?: ReactNode;
  className?: string;
}) {
  const match = /language-(\w+)/.exec(className || "");
  const lang = match ? match[1] : "";

  if (!lang) {
    // Inline code
    return (
      <code className="rounded bg-black/5 px-2 py-1 text-sm text-[#2b2e36] dark:bg-white/10 dark:text-slate-200">
        {children}
      </code>
    );
  }

  // Block code with syntax highlighting
  const code = String(children || "").replace(/\n$/, "");
  
  // Attempt to highlight, fall back to plain code block on error
  let html: string;
  try {
    html = await highlightCode(code, lang);
  } catch (error) {
    console.error("Failed to highlight code:", error);
    // Return plain code block as fallback
    return (
      <pre className="mt-6 overflow-x-auto rounded-2xl bg-[#f6f8fa] p-5 text-sm text-[#24292f] dark:bg-[#0f1116] dark:text-slate-50">
        <code>{code}</code>
      </pre>
    );
  }

  return (
    <div
      className="syntax-highlighter mt-6 overflow-x-auto rounded-2xl [&_pre]:!bg-transparent [&_pre]:p-5 [&_pre]:text-sm"
      dangerouslySetInnerHTML={{ __html: html }}
    />
  );
}
