export function modulify(obj) {
  return Object.entries(
    Object.getOwnPropertyDescriptors(Object.getPrototypeOf(obj))
  ).reduce(
    (acc, [k, descr]) =>
      k === "constructor" || descr.get
        ? acc
        : ((acc[k] = obj[k].bind(obj)), acc),
    {}
  );
}
