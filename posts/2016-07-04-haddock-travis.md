---
title: Continuous Haddocks
date: Tue Jul  5 02:07:11 UTC 2016
author: Eduardo Trujillo
uuid: 60456ddb-e31d-4ae1-8e44-787c7da1e804
---

On a previous post, I covered how to setup continuous integration for Haskell
projects using a combination of Stack, Travis CI, and Docker. But what about
documentation?

![Sweet auto-generated docs!](/images/posts/haddocks.png)

If you already have CI setup for your project with Stack and Travis CI, it is
actually pretty easy. In fact, you can make use of GitHub's Pages feature,
which hosts statics sites based on the content of the `gh-pages` branch to host
your documentation for free.

The following is a bash script I've been using on a couple of projects on
GitHub. It takes care of collecting the documentation and coverage reports
generated while building the application, triggered using Stack's `--haddock`
and `--coverage` respectively.

```bash
#!/bin/bash

# Make sure you install the Travis CLI and encrypt a GitHub API token with
# access to the repository: `travis encrypt GH_TOKEN=xxxxxxx --add`.

# Copy haddocks to a separate directory.
cp -R "$(stack path --local-doc-root)" ../gh-pages
cp -R "$(stack path --local-hpc-root)" ../gh-pages
cd ../gh-pages

# Set identity.
git config --global user.email "travis@travis-ci.org"
git config --global user.name "Travis"

# Add branch.
git init
git remote add origin https://${GH_TOKEN}@github.com/${TRAVIS_REPO_SLUG}.git > /dev/null
git checkout -B gh-pages

# Push generated files.
git add .
git commit -m "Haddocks updated" git push origin gh-pages -fq > /dev/null
```

To do its job, the script relies on some neat tricks/hacks to keep the process
as simple as possible:

- **Stack:** When using `--haddock` and `--coverage` flags, Stack will place
  documentation and coverage reports on specific paths. You can query these
  paths using `stack path`. On the script above, we use special flags so the
  output of the program is a single line with the requested path. This avoids
  having to think about or trying to find where the compiler placed the
  documentation and related files.

- **GitHub API Tokens:** Managing SSH keys inside a build job is probably
  something doable but probably not easy. Thankfully, GitHub allows you to push
  commits to a repository using just an API token.

- **Travis CI encrypted variables:** This allows us to conveniently store the
  aforementioned token in a secure manner and easily access it as an
  environment variable while the job runs. We do have to use `> /dev/null` on a
  couple of places so the key is not leaked on build logs.

- **Bare Git branch:** Given that keeping track of history is not a priority
  and could break the build process if somebody else pushed to the
  documentation branch, we simply keep a single commit on the `gh-pages`
  branch. One can easily do this by initializing a new repository, committing,
  and force-pushing into the branch.

If you would like to see a public working example, checkout
[this repository][1] and its [build logs][2] on Travis CI. The resulting
documentation is available as a [GitHub pages website][3] and
[coverage reports][4] can be found under `/hpc` on the site.

[1]: https://github.com/etcinit/craze/blob/master/.travis/assemble-docs.sh
[2]: https://travis-ci.org/etcinit/craze
[3]: https://etcinit.github.io/craze
[4]: https://etcinit.github.io/craze/hpc
