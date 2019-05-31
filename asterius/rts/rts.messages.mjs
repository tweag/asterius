import {format } from "util";
export class Messages {
    constructor(memory, fs) {
        this.memory = memory;
        this.fs = fs
        Object.seal(this);
    }

    debugBelch2(fmt, arg) {
        let s = format(this.memory.strLoad(fmt), this.memory.strLoad(arg));
        console.error(s);
        this.fs.writeSync(this.fs.stderr(), s);
    }

}
