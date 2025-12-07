/* Polyfill the DOM File class for Node.js runtimes that only expose Blob. */
if (typeof File === 'undefined') {
  const { Blob } = require('buffer');

  class FilePolyfill extends Blob {
    constructor(bits, name, options = {}) {
      super(bits, options);
      this.name = typeof name === 'string' ? name : String(name ?? '');
      this.lastModified = typeof options.lastModified === 'number'
        ? options.lastModified
        : Date.now();
    }

    get [Symbol.toStringTag]() {
      return 'File';
    }
  }

  global.File = FilePolyfill;
}
