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
      const range = self.getSelectionRange(self);

      if (range) {
        console.log(range);

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

  getSelectionRange(container) {
    const sel = window.getSelection ? window.getSelection() : document.selection;

    if (sel && sel.rangeCount > 0) {
      // create a copy so we don't interfere with user selection range
      const range = sel.getRangeAt(0).cloneRange();

      // create a range which wraps the whole selection container
      const containerRange = document.createRange();
      containerRange.selectNodeContents(container);

      // selection range is within bounds, all good
      if (container.contains(range.startContainer) &&
        container.contains(range.endContainer)) {
          return range;
      }

      // selection range is either completely before or completely after
      if (range.compareBoundaryPoints(Range.START_TO_END, containerRange) <= 0 ||
        range.compareBoundaryPoints(Range.END_TO_START, containerRange) >= 0) {
        return null;
      }

      // selection range starts before our container
      if (!container.contains(range.startContainer)) {
        range.setStart(containerRange.startContainer, 0);
      }

      // selection range ends after our container
      if (!container.contains(range.endContainer)) {
        range.setEndAfter(containerRange.endContainer);
      }

      return range;
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
