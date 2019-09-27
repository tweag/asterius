function newThunk(f) {
  let t = () => {
    const r = f();
    t = () => r;
    return r;
  };
  return () => t();
}

function newNode() {
  let r,
    p = new Promise(resolve => {
      r = resolve;
    });
  return { promise: p, resolve: r, next: newThunk(newNode) };
}

export class Channel {
  constructor() {
    this.takeNode = newNode();
    this.putNode = this.takeNode;
    Object.seal(this);
  }

  take() {
    const r = this.takeNode.promise;
    this.takeNode = this.takeNode.next();
    return r;
  }

  put(r) {
    this.putNode.resolve(r);
    this.putNode = this.putNode.next();
  }
}
