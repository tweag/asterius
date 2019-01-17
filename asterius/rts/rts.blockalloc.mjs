import * as settings from "./rts.settings.mjs";

export class BlockAlloc {
  constructor(memory) {
    this.memory = memory;
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
    let mblock, block, bd, block_addr;
    if (this.lastBlock + n <= settings.blocks_per_mblock) {
      mblock = this.lastMBlock;
      block = this.lastBlock;
      this.lastBlock += n;
    } else if (n <= settings.blocks_per_mblock) {
      this.lastMBlock = this.mblockAlloc.getMBlocks(1);
      this.lastBlock = n;
      mblock = this.lastMBlock;
      block = 0;
    } else {
      mblock = this.mblockAlloc.getMBlocks(
          1 + Math.ceil((n - settings.blocks_per_mblock) /
                        (settings.mblock_size / settings.block_size)));
      block = 0;
      this.lastMBlock = this.mblockAlloc.getMBlocks(1);
      this.lastBlock = 0;
    }
    bd = mblock + settings.offset_first_bdescr +
         (block << Math.log2(settings.sizeof_bdescr));
    block_addr = mblock + settings.offset_first_block +
                 (block << Math.log2(settings.block_size));
    this.memory.i64Store(bd + settings.offset_bdescr_start, block_addr);
    this.memory.i64Store(bd + settings.offset_bdescr_free, block_addr);
    this.memory.i64Store(bd + settings.offset_bdescr_link, 0);
    this.memory.i32Store(bd + settings.offset_bdescr_blocks, n);
    return bd;
  }
}
