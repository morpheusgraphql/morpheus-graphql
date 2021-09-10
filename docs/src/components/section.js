import React, { useContext } from "react";
import { NavContext } from "./nav-context";

export const Section = ({ id, children }) => {
  const [, setItem] = useContext(NavContext);

  setItem({ id, children });

  return (
    <h2
      id={`${id}`}
      style={{ color: "black", textDecoration: "none", padding: "0.1rem 0rem" }}
    >
      {children}
    </h2>
  );
};
