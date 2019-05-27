export class Messages {
    constructor(memory) {
        this.memory = memory;
        Object.seal(this);
    }

    debugBelch2(fmt, arg) {
        console.error(this.memory.strLoad(fmt), this.memory.strLoad(arg));
    }

}
