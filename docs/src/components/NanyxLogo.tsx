export default function NanyxLogo(props: React.SVGProps<SVGSVGElement>) {
  return (
    <svg
      xmlns="http://www.w3.org/2000/svg"
      viewBox="0 0 128 128"
      role="img"
      aria-label="NX logo"
      {...props}
    >
      <defs>
        <linearGradient id="nxGrad" x1="14" y1="14" x2="114" y2="114" gradientUnits="userSpaceOnUse">
          <stop offset="0" stopColor="#2F80FF" />
          <stop offset="0.55" stopColor="#6A5CFF" />
          <stop offset="1" stopColor="#B04CFF" />
        </linearGradient>
      </defs>
      <g fill="none" stroke="url(#nxGrad)" strokeLinecap="round">
        <circle cx="64" cy="64" r="50" strokeWidth="4" />
        <circle cx="64" cy="64" r="46" strokeWidth="1" />
      </g>
      <g fill="url(#nxGrad)">
        <g transform="translate(-6,0)">
          <path d="M34 88 V40 H44 L60 64 V40 H70 V88 H60 L44 64 V88 Z" />
        </g>
        <g transform="translate(8,0)">
          <path d="M94 88 H82 L74 74 L66 88 H54 L70 64 L54 40 H66 L74 54 L82 40 H94 L78 64 Z" />
        </g>
      </g>
    </svg>
  );
}
