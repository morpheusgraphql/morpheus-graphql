import { Logo } from "./logo";
import Link from "next/link";

const headerLink = {
  color: "hsl(211deg 23% 20%)",
  textDecoration: "none",
  padding: "1rem 1rem",
  fontWeight: 600,
};

type ItemProps = {
  href: string;
  children: React.ReactNode;
};

const Item: React.FC<ItemProps> = ({ href, children }) => (
  <div style={headerLink}>
    <Link href={href}>{children}</Link>
  </div>
);

const Header = () => (
  <div>
    <Logo />
    <div
      style={{
        margin: "0 auto",
        padding: "20px 30px",
        display: "flex",
        flexDirection: "row",
        background: "rgb(215 239 253 / 55%)",
        textAlign: "center",
        alignItems: "center",
        position: "relative",
      }}
    >
      <h1 style={{ margin: 0 }}>
        <Item href="/">Morpheus GraphQL</Item>
      </h1>
      <Item href="/server">Server</Item>
      <Item href="/resolving">Resolving</Item>
      <Item href="/client">Client</Item>
      <Item href="/examples">Examples</Item>
    </div>
  </div>
);

export default Header;
