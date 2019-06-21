export class StableNameManager {
  constructor() {
    this.spt = new Map();
    Object.freeze(this);
  }

  lookupStableName(ptr) {
      return ptr;
  }

}
