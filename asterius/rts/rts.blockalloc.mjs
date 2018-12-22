import * as settings from "./rts.settings.mjs";

export class BlockAlloc {
  constructor() {
    this.mblockAlloc = null;
    this.lastMBlock = null;
    this.lastBlock = null;
    Object.seal(this);
  }

  init(mblockAlloc) {
    this.mblockAlloc = mblockAlloc;
    this.lastMBlock = mblockAlloc.getMBlocks(1);
    this.lastBlock = 0;
  }

  allocGroup(n) {
    let mblock, block;
    if (this.lastBlock + n <= settings.blocksPerMBlock) {
      mblock = this.lastMBlock;
      block = this.lastBlock;
      this.lastBlock += n;
    } else if (n <= settings.blocksPerMBlock) {
      this.lastMBlock = this.mblockAlloc.getMBlocks(1);
      this.lastBlock = n;
      mblock = this.lastMBlock;
      block = 0;
    } else {
      mblock = this.mblockAlloc.getMBlocks(
        1 +
          Math.ceil(
            (n - settings.blocksPerMBlock) /
              (settings.mblockSize / settings.blockSize)
          )
      );
      block = 0;
      this.lastMBlock = this.mblockAlloc.getMBlocks(1);
      this.lastBlock = 0;
    }
    return (
      mblock + settings.firstBdescr + (block << Math.log2(settings.bdescrSize))
    );
  }
}
