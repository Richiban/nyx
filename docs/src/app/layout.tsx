import type { Metadata } from "next";
import { Audiowide } from "next/font/google";
import "./globals.css";

const audiowide = Audiowide({
  weight: "400",
  variable: "--font-heading",
  subsets: ["latin"],
});


export const metadata: Metadata = {
  title: "Nanyx Docs",
  description: "Nanyx language documentation and guides",
};

export default function RootLayout({
  children,
}: Readonly<{
  children: React.ReactNode;
}>) {
  return (
    <html lang="en">
      <body
        className={`${audiowide.variable} antialiased`}
      >
        {children}
      </body>
    </html>
  );
}
