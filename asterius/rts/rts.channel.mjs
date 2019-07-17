function newThunk(f) {
  let t = () => {
    const r = f();
    t = () => r;
    return r;
  };
  return () => t();
}

function newNode() {
  let r = undefined,
    p = new Promise(resolve => (r = resolve));
  return { promise: p, resolve: r, next: newThunk(newNode) };
}

export class Channel {
  constructor() {
    this.takeNode = newNode();
    this.putNode = this.takeNode;
    Object.seal(this);
  }

  take() {
    return this.takeNode.promise.then(
      r => {
        this.takeNode = this.takeNode.next();
        return r;
      },
      err => {
        this.takeNode = this.takeNode.next();
        return Promise.reject(err);
      }
    );
  }

  put(r) {
    this.putNode.resolve(r);
    this.putNode = this.putNode.next();
  }
}
