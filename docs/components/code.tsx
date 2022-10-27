import React from "react";

type ItemProps = {
  children: React.ReactNode;
  file: string;
};

const Code: React.FC<ItemProps> = ({ children, file }) => (
  <div
    style={{
      background: "var(--background)",
      borderRadius: "5px",
      margin: "20px 0",
    }}
  >
    <p
      style={{
        margin: "0",
        padding: "5px",
        textAlign: "start",
        fontFamily: "monospace",
        color: "var(--foreground)",
        borderBottom: "1px solid var(--comment)",
        fontSize: "12px",
      }}
    >
      {file}
    </p>
    {children}
  </div>
);

export { Code };
