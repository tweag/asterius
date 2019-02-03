import * as settings from "./rts.settings.mjs";
import { Memory } from "./rts.memory.mjs";

export class MBlockAlloc {
  constructor() {
    this.memory = undefined;
    this.staticMBlocks = undefined;
    this.capacity = undefined;
    this.size = undefined;
    this.freeList = [];
    Object.seal(this);
  }

  init(memory, static_mblocks) {
    this.memory = memory;
    this.staticMBlocks = static_mblocks;
    this.capacity = this.memory.buffer.byteLength / settings.mblock_size;
    this.size = this.capacity;
  }

  getMBlocks(n) {
    if (this.size + n > this.capacity) {
      const d = Math.max(n, this.capacity);
      this.memory.grow(d * (settings.mblock_size / settings.pageSize));
      this.capacity += d;
    }
    const prev_size = this.size;
    this.size += n;
    return Memory.tagData(prev_size * settings.mblock_size);
  }

  allocMegaGroup(n) {
    const req_blocks =
        n == 1 ? settings.blocks_per_mblock
               : settings.blocks_per_mblock +
                     (settings.mblock_size / settings.block_size) * (n - 1);
    for (let i = 0; i < this.freeList.length; ++i) {
      const bd = this.freeList[i],
            blocks = this.memory.i32Load(bd + settings.offset_bdescr_blocks);
      if (req_blocks < blocks) {
        this.memory.i32Store(bd + settings.offset_bdescr_blocks, req_blocks);
        const rest_bd = bd + (settings.mblock_size * n),
              rest_start = rest_bd - settings.offset_first_bdescr +
                           settings.offset_first_block;
        this.memory.i64Store(rest_bd + settings.offset_bdescr_start,
                             rest_start);
        this.memory.i64Store(rest_bd + settings.offset_bdescr_free, rest_start);
        this.memory.i64Store(rest_bd + settings.offset_bdescr_link, 0);
        this.memory.i32Store(rest_bd + settings.offset_bdescr_blocks,
                             blocks - req_blocks -
                                 ((settings.mblock_size / settings.block_size) -
                                  settings.blocks_per_mblock));
        this.freeList.splice(i, 1, rest_bd);
        return bd;
      }
      if (req_blocks == blocks) {
        this.freeList.splice(i, 1);
        return bd;
      }
    }
    const mblock = this.getMBlocks(n),
          bd = mblock + settings.offset_first_bdescr,
          block_addr = mblock + settings.offset_first_block;
    this.memory.i64Store(bd + settings.offset_bdescr_start, block_addr);
    this.memory.i64Store(bd + settings.offset_bdescr_free, block_addr);
    this.memory.i64Store(bd + settings.offset_bdescr_link, 0);
    this.memory.i32Store(bd + settings.offset_bdescr_blocks, req_blocks);
    return bd;
  }

  freeSegment(l_end, r) {
    if (l_end < r) {
      this.memory.memset(l_end, 0, r - l_end);
      const bd = l_end + settings.offset_first_bdescr,
            start = l_end + settings.offset_first_block,
            mblocks = (r - l_end) / settings.mblock_size,
            blocks = mblocks == 1
                         ? settings.blocks_per_mblock
                         : settings.blocks_per_mblock +
                               (settings.mblock_size / settings.block_size) *
                                   (mblocks - 1);
      this.memory.i64Store(bd + settings.offset_bdescr_start, start);
      this.memory.i64Store(bd + settings.offset_bdescr_free, start);
      this.memory.i64Store(bd + settings.offset_bdescr_link, 0);
      this.memory.i32Store(bd + settings.offset_bdescr_blocks, blocks);
      this.freeList.push(bd);
    }
  }

  preserveMegaGroups(bds) {
    this.freeList = [];
    const sorted_bds = Array.from(bds).sort((bd0, bd1) => bd0 - bd1);
    this.freeSegment(
        Memory.tagData(settings.mblock_size * this.staticMBlocks),
        sorted_bds[0] - settings.offset_first_bdescr);
    let l_start, l_blocks, l_end, r;
    for (let i = 0; i < sorted_bds.length; ++i) {
      l_start = Number(
          this.memory.i64Load(sorted_bds[i] + settings.offset_bdescr_start));
      l_blocks =
          this.memory.i32Load(sorted_bds[i] + settings.offset_bdescr_blocks);
      l_end = l_start + (settings.block_size * l_blocks);
      r = sorted_bds[i + 1] - settings.offset_first_bdescr;
      if (r) this.freeSegment(l_end, r);
    }
    this.freeSegment(l_end,
                     Memory.tagData(settings.mblock_size * this.capacity));
    this.freeList.sort(
        (bd0, bd1) => this.memory.i32Load(bd0 + settings.offset_bdescr_blocks) -
                  this.memory.i32Load(bd1 + settings.offset_bdescr_blocks));
  }
}
