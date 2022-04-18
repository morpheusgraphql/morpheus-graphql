import React from "react";

export type NavItem = {
  id: string;
  level: 1 | 2 | 3;
  children: JSX.Element[];
};

export const NavContext = React.createContext<
  [NavItem[], (_: NavItem) => void]
>([[], () => undefined]);

const notMember = (items: NavItem[], item: NavItem) =>
  !items.find(({ id }) => id === item.id);

export const NavContextProvider: React.FC<{ children: React.ReactNode }> = ({
  children,
}) => {
  const [items, setItem] = React.useState<NavItem[]>([]);

  const register = (item: NavItem) =>
    notMember(items, item) ? setItem([item, ...items]) : undefined;

  return (
    <NavContext.Provider value={[items, register]}>
      {children}
    </NavContext.Provider>
  );
};
