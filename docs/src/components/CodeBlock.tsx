import { createHighlighter } from "shiki";
import nyxGrammar from "../../public/grammars/nyx.tmLanguage.json";

// We'll cache the highlighter instance
let highlighterPromise: Promise<any> | null = null;

async function getHighlighter() {
  if (!highlighterPromise) {
    highlighterPromise = (async () => {
      const highlighter = await createHighlighter({
        themes: ["github-dark"],
        langs: ["javascript", "typescript", "bash"],
      });
      
      // Load the Nyx language grammar
      await highlighter.loadLanguage({
        name: "nyx",
        scopeName: "source.nyx",
        ...nyxGrammar,
      } as any);
      
      return highlighter;
    })();
  }
  return highlighterPromise;
}

export async function CodeBlock({
  children,
  className,
  ...props
}: {
  children: string;
  className?: string;
  [key: string]: any;
}) {
  const match = /language-(\w+)/.exec(className || "");
  const lang = match ? match[1] : "";

  if (!lang) {
    // Inline code
    return (
      <code
        className="rounded bg-black/5 px-2 py-1 text-sm text-[#2b2e36]"
        {...props}
      >
        {children}
      </code>
    );
  }

  // Block code with syntax highlighting
  const code = String(children).replace(/\n$/, "");
  
  try {
    const highlighter = await getHighlighter();
    const html = highlighter.codeToHtml(code, {
      lang: lang,
      theme: "github-dark",
    });

    return (
      <div
        className="syntax-highlighter mt-6 overflow-x-auto rounded-2xl [&_pre]:!bg-transparent [&_pre]:p-5 [&_pre]:text-sm"
        dangerouslySetInnerHTML={{ __html: html }}
      />
    );
  } catch (error) {
    console.error("Failed to highlight code:", error);
    // Fallback to plain code block
    return (
      <pre className="mt-6 overflow-x-auto rounded-2xl bg-[#0f1116] p-5 text-sm text-slate-50">
        <code>{code}</code>
      </pre>
    );
  }
}
