import { range, transpose } from "ramda";
import { Table } from "../utils/types";

const fill = (size: number): string =>
  range(0, size)
    .map(() => " ")
    .join("");

export const formatTable = (table: Table) => {
  const sizes = transpose(table).map((colum) =>
    Math.max(...colum.map((item) => item.length))
  );

  return table.map((row) =>
    row
      .map((item, i) => item + fill(sizes[i] - item.length))
      .join("  ")
      .trim()
  );
};
