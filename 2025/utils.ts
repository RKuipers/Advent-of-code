export const modulo = (x: number, y: number) => {
  const r = x % y;
  return r > 0 ? r : r + y;
};
