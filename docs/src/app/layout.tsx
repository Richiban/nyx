import type { Metadata } from "next";
import { JetBrains_Mono, Literata, Space_Grotesk, Audiowide } from "next/font/google";
const audiowide = Audiowide({
  variable: "--font-audiowide",
  subsets: ["latin"],
});
import "./globals.css";

const spaceGrotesk = Space_Grotesk({
  variable: "--font-heading",
  subsets: ["latin"],
});

const literata = Literata({
  variable: "--font-body",
  subsets: ["latin"],
});

const jetBrainsMono = JetBrains_Mono({
  variable: "--font-mono",
  subsets: ["latin"],
});

export const metadata: Metadata = {
  title: "Nanyx Docs",
  description: "Nanyx language documentation and guides",
};

  children,
}: Readonly<{
    children: React.ReactNode;
}>) {
  return (
    <html lang="en">
      <body
        className={`${spaceGrotesk.variable} ${literata.variable} ${jetBrainsMono.variable} ${audiowide.variable} antialiased`}
      >
        {children}
      </body>
    </html>
  );
}
