import { useContext } from "react";
import { NavContext, NavItem } from "./nav-context";

const style = {
  color: "black",
  textDecoration: "none",
  padding: "0.1rem 0rem",
};

export const Section: React.FC<NavItem> = ({ id, children, level = 1 }) => {
  const [, setItem] = useContext(NavContext);

  setItem({ id, level, children });

  return level === 1 ? (
    <h2 id={`${id}`} style={style}>
      {children}
    </h2>
  ) : (
    <h3 id={`${id}`} style={style}>
      {children}
    </h3>
  );
};

export default Section;
