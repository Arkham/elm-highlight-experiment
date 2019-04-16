class ElmHighlight extends HTMLElement {
  constructor(...args) {
    const self = super(...args);

    self._content = null;

    // TODO: none of these event names are standard, and they may clash with
    // ones that the browser emits. What's the best way to do this safely?

    // TODO: this needs prettier on the whole thing

    // TODO: probably needs like flow or TS checking as well

    // add event handler for selections
    document.addEventListener('selectionchange', function(event) {
      // if (event.target.activeElement !== self) { return; }

      const range = self.getSelectionRange();
      console.log(range);

      if (range) {
        self.dispatchEvent(new CustomEvent('select', { detail: {
          start: {
            node: range.startContainer,
            offset: self.offsetUntil(range.startContainer) + range.startOffset
          },
          end: {
            node: range.endContainer,
            offset: self.offsetUntil(range.endContainer) + range.endOffset
          },
          originalEvent: range,
        }}));
      }
    });

    return self;
  }

  get content() {
    return this._content;
  }

  set content(content) {
    if (this._content !== content) {
      this._content = content;
      this.clearSelection();
    }
  }

  getSelectionRange() {
    const sel = window.getSelection ? window.getSelection() : document.selection;

    if (sel && sel.rangeCount > 0) {
      return sel.getRangeAt(0);
    }
  }

  clearSelection() {
    const sel = window.getSelection ? window.getSelection() : document.selection;

    if (sel) {
      if (sel.removeAllRanges) {
        sel.removeAllRanges();
      } else if (sel.empty) {
        sel.empty();
      }
    }
  }

  offsetUntil(endingAt) {
    var stack = [this];
    var total = 0;

    while (stack.length !== 0) {
      var current = stack.pop();

      if (current === endingAt) {
        // first, have we reached where we want to go? Get outta here!
        break;
      } else if (current.nodeType === Node.TEXT_NODE) {
        // next, are we looking at a text node? Cool, add it to the total!
        total += current.length;
      } else if (!current.hasChildNodes()) {
        // next, check out nodes which don't have any children. This'll be things
        // like images and iframe embeds, and we'll count them as having length 1.
        //
        // TODO: is this the right thing to do? I'm just making it this way since
        // Quill seems to.
        total += 1;
      } else if (current.hasChildNodes()) {
        // shove the remaining children on the stack in reverse order (so we pop
        // them start to end)
        for (var i = current.childNodes.length - 1; i >= 0; i--) {
          stack.push(current.childNodes[i]);
        }
      }
    }

    return total;
  }
}

customElements.define('elm-highlight', ElmHighlight);
