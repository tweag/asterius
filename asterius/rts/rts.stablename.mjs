// https://github.com/ghc/ghc/blob/bf73419518ca550e85188616f860961c7e2a336b/includes/rts/StableName.h
// https://github.com/ghc/ghc/blob/43967c0c7d2d0110cfc5f9d64a7dab3a3dda8953/rts/StableName.c
export class StableNameManager {
  constructor() {
    this.spt = new Map();
    Object.freeze(this);
  }

  makeStableName(obj) {
      return obj;
  }

}
