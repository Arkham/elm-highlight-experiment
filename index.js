// index.js

import { Elm } from './src/Main.elm'
import './src/elmHighlight';

Elm.Main.init({
  node: document.querySelector('main')
})
