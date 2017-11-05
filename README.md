# Chromabits

Blog static site generator and content server

## Development

Previously you needed a few tools to build this blog (Node, Bower, Gulp). It's
now possible to build and host this blog using just Git and Haskell.

First, make sure all submodules in the repository are initialized and up to
date. This is how we pull external dependencies like Foundation and Font
Awesome.

Next, use `stack` to build the blog generator and the server:

```
stack build -j8
stack exec blog hakyll build
stack exec server
```

You should now be able to go to `http://localhost:9090` to load the blog.
