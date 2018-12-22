export function modulify(obj) {
  return Object.getOwnPropertyNames(Object.getPrototypeOf(obj))
      .reduce((acc, k) => k === "constructor" ? acc : (acc[k] = obj[k].bind(obj), acc),
              {});
};
