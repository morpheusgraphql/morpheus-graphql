import { Logo } from "./logo";
import Link from "next/link";

const headerLink = {
  color: "hsl(211deg 23% 20%)",
  textDecoration: "none",
  padding: "1rem 1rem",
  fontWeight: 600,
};

const Item = ({ href, children }) => (
  <div style={headerLink}>
    <Link href={href}>{children}</Link>
  </div>
);

const Header = ({ siteTitle }) => (
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
      <Item href="/">
        <h1 style={{ margin: 0 }}>Morpheus GraphQL</h1>
      </Item>
      <Item href="/about">About</Item>
      <Item href="/server">Server</Item>
      <Item href="/client">Client</Item>
      <Item href="/examples">Examples</Item>
      <Item href="/named-resolvers">Named Resolvers</Item>
    </div>
  </div>
);

export default Header;
