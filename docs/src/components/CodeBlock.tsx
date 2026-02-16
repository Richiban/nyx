import { createHighlighter } from "shiki";
import nyxGrammar from "../../public/grammars/nyx.tmLanguage.json";

let highlighterInstance: Awaited<ReturnType<typeof createHighlighter>> | null = null;

async function getHighlighter() {
  if (!highlighterInstance) {
    highlighterInstance = await createHighlighter({
      themes: ["github-dark"],
      langs: [],
    });

    // Load the Nyx language
    await highlighterInstance.loadLanguage({
      id: "nyx",
      scopeName: "source.nyx",
      grammar: nyxGrammar as any,
      aliases: ["nyx"],
    } as any);
  }
  return highlighterInstance;
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
  const highlighter = await getHighlighter();
  const code = String(children).replace(/\n$/, "");
  
  const html = highlighter.codeToHtml(code, {
    lang: lang === "nyx" ? "nyx" : lang,
    theme: "github-dark",
  });

  return (
    <div
      className="syntax-highlighter mt-6 overflow-x-auto rounded-2xl [&_pre]:!bg-transparent [&_pre]:p-5 [&_pre]:text-sm"
      dangerouslySetInnerHTML={{ __html: html }}
    />
  );
}
