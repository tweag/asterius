class Event {
  constructor(lv, ev) {
    this.time = new Date();
    this.level = lv;
    this.event = ev;
    Object.freeze(this);
  }
}

export class EventLogManager {
  constructor() {
    this.events = [];
    this.enabled = true;
    this.onEvent = () => {};
    Object.seal(this);
  }

  isEnabled() {
    return this.enabled;
  }

  setEnabled(f) {
    this.enabled = Boolean(f);
  }

  log(lv, _ev) {
    if (this.enabled) {
      const ev = new Event(lv, _ev);
      this.events.push(ev);
      this.onEvent(ev);
    }
  }

  logInfo(ev) {
    this.log("INFO", ev);
  }

  logError(ev) {
    this.log("ERROR", ev);
  }

  logEvent(ev) {
    this.log("EVENT", ev);
  }
}
