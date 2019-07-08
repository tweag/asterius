export class Messages {
    constructor(memory, fs) {
        this.memory = memory;
        this.fs = fs
        Object.seal(this);
    }

    debugBelch2(fmt, arg) {
        const s = `${this.memory.strLoad(arg)}\n`;
        console.error(s);
        this.fs.writeSync(this.fs.stderr(), s);
    }

}
