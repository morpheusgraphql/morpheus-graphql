import React from "react";

export const NavContext = React.createContext([[], () => undefined]);

export const NavContextProvider = ({ children }) => {
  const [items, setItem] = React.useState([]);

  const register = (item) => {
    if (items.find((x) => x.id === item.id)) {
      return undefined;
    }

    setItem([item, ...items]);
  };

  return (
    <NavContext.Provider value={[items, register]}>
      {children}
    </NavContext.Provider>
  );
};
