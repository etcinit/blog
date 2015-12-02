---
title: Mousetrap + React.js
tags: react, mousetrap, js
uuid: 583db650-ee44-4c2e-9780-6af560872f0e
---

Here's a little mixin I've been using on React.js for interacting with the Mousetrap library (a library for handling keyboard shortcuts on the browser). It automatically unbinds shortcuts once the component unmounts and provides some convenience methods for avoiding interacting with Mousetrap directly on m components.

> NOTE: The following code is formatted for Browserify. You might need to make some changes for using it without Browserify

To use it, require the module and add the mixing to your component:

```javascript
// ...
var MoustrapMixin = require('/path/to/MoustrapMixin.jsx');

MyComponent = React.createClass({
    // ...
    mixins: [MoustrapMixin],

    componentDidMount: function () {
        this.bindShortcut('esc', function () { // Handle shortcut });
    }
    // ...
});
```

### MoustrapMixin.jsx:

<script src="https://gist.github.com/etcinit/7859b3380ea75020e130.js"></script>
