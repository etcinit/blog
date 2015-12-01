---
title: Checkboxes and ZURB Foundation
tags: foundation, scss
---

The current version ZURB's Foundation (A CSS framework) doesn't style form checkboxes and the final look is always the browser default, which can look good or bad depending on your OS.

I like to have a more consistent look on my apps, especially on those that use checkboxes for selecting which items from a table to apply certain action to.

I use the following SCSS to style checkbox input elements. The original code was based from the look of checkboxes in Gmail although it was slightly more rounded. I've adapted it so that it fits better in Foundation (square corners), plus it will also use your `$primary-color` when it is highlighted:

<script src="https://gist.github.com/etcinit/86464e5a9e3d362e24a6.js"></script>

The final result should look something like this:

<img src="http://assets.chromabits.com/posts/foundation-checkbox.png" style="max-width: 200px; margin-left: auto; margin-right: auto">
