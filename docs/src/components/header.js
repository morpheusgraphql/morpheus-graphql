import { Link } from "gatsby";
import PropTypes from "prop-types";
import React from "react";
import Image from "./image";
import { Logo } from "./logo";

const headerLink = {
  color: "white",
  textDecoration: "none",
  padding: "1rem 1rem",
};

const Header = ({ siteTitle }) => (
  <div
    style={{
      width: "100%",
      overflow: "clip",
      position: "relative",
      marginBottom: "4rem",
    }}
  >
    <div
      style={{
        position: "absolute",
        top: "0",
        left: "0",
        height: "100%",
        width: "100%",
        zIndex: -2,
        overflow: "clip",
      }}
    >
      <Image />
    </div>
    <Logo />
    <div
      style={{
        margin: "0 auto",
        padding: "20px 30px",
        display: "flex",
        flexDirection: "row",
        background: "#ffffff17",
        textAlign: "center",
        alignItems: "center",
        position: "relative",
      }}
    >
      <Link to="/" style={headerLink}>
        <h1 style={{ margin: 0 }}>{siteTitle}</h1>
      </Link>
      <Link to="/about" style={headerLink}>
        About
      </Link>
      <Link to="/client" style={headerLink}>
        Morpheus Client
      </Link>
      <Link to="/examples" style={headerLink}>
        Examples
      </Link>
      <Link to="/named-resolvers" style={headerLink}>
        Named Resolvers
      </Link>
      <Link to="/type-system" style={headerLink}>
        Type System
      </Link>
    </div>
  </div>
);

Header.propTypes = {
  siteTitle: PropTypes.string,
};

Header.defaultProps = {
  siteTitle: "",
};

export default Header;
