export class Channel {
  constructor() {
    this.init();
    Object.seal(this);
  }

  init() {
    this.promise = new Promise((resolve, reject) => {
      this.resolve = resolve;
      this.reject = reject;
    }).then(r => (this.init(), r), err => (this.init(), Promise.reject(err)));
  }

  take() {
    return this.promise;
  }
}
