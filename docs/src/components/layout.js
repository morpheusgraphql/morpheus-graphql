import React from "react";
import PropTypes from "prop-types";
import { StaticQuery, graphql, Link } from "gatsby";
import { defineCustomElements as deckDeckGoHighlightElement } from "@deckdeckgo/highlight-code/dist/loader";
import Header from "./header";
import "./layout.css";
import { NavContext, NavContextProvider } from "../components/nav-context";

deckDeckGoHighlightElement();

const NavLink = ({ to, children }) => (
  <Link
    to={`${window.location.pathname}#${to}`}
    style={{
      color: "rgb(39, 51, 63)",
      textDecoration: "none",
      padding: "0.1rem 0rem",
    }}
  >
    {children}
  </Link>
);

const Navigation = () => {
  const [items] = React.useContext(NavContext);
  return (
    <nav
      style={{
        gridColumn: "1 / 3",
        display: "flex",
        paddingBottom: 5,
        flexDirection: "column",
        position: "sticky",
        top: "5vh",
        height: "80vh",
      }}
    >
      {items.map(({ id, children }) => (
        <NavLink to={id} key={id}>
          {children}
        </NavLink>
      ))}
    </nav>
  );
};

const Layout = ({ children, ...props }) => (
  <StaticQuery
    query={graphql`
      query SiteTitleQuery {
        site {
          siteMetadata {
            title
          }
        }
      }
    `}
    render={(data) => (
      <>
        <NavContextProvider value={[]}>
          <Header siteTitle={data.site.siteMetadata.title} />
          <div
            style={{
              width: "100%",
              background: "rgb(215 239 253 / 65%)",
            }}
          >
            <div
              style={{
                display: "grid",
                gridTemplateColumns: "repeat(12, 1fr)",
                gap: "2em",
                maxWidth: 1260,
                margin: "0 auto",
                padding: "8rem 2rem",
                background: "rgb(243 251 255 / 81%)",
                border: "2px solid white",
                borderRadius: 5,
              }}
            >
              {<Navigation />}
              <div
                style={{
                  gridColumn: "3 / 13",
                }}
              >
                {children}
              </div>
            </div>
          </div>
        </NavContextProvider>
      </>
    )}
  />
);

Layout.propTypes = {
  children: PropTypes.node.isRequired,
};

export default Layout;
