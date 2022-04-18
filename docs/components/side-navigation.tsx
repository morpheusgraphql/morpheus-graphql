import { useContext } from "react";
import Link from "next/link";
import { NavContext } from "./nav-context";

const linkSytle = (level: number) =>
  ({
    color: "rgb(39, 51, 63)",
    textDecoration: "none",
    padding: "0.1rem 0rem",
    paddingLeft: (level - 1) * 16,
    fontSize: 8 + 20 / (level + 1),
  } as const);

const navStyle = {
  gridColumn: "1 / 3",
  display: "flex",
  paddingBottom: 5,
  flexDirection: "column",
  position: "sticky",
  top: "5vh",
  height: "80vh",
} as const;

const url = (h: string) => `${window.location.pathname}#${h}`;

type NavLinkProps = {
  level: number;
  children: React.ReactNode;
  to: string;
};

const NavLink: React.FC<NavLinkProps> = ({ to, children, level }) => (
  <Link href={url(to)} as={url(to)}>
    <a style={linkSytle(level)}>{children}</a>
  </Link>
);

const SideNavigation = () => {
  const [items] = useContext(NavContext);
  return (
    <nav style={navStyle}>
      {items.map(({ id, children, level }) => (
        <NavLink to={id} key={id} level={level}>
          {children}
        </NavLink>
      ))}
    </nav>
  );
};

export { SideNavigation };
