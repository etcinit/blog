---
title: Continuous integration for Haskell projects with Docker
date: Wed April 20 14:21:00 EDT 2016
author: Eduardo Trujillo
uuid: ccdac18a-4592-4560-9505-8940bb69df46
---

At my current job, we are starting to adopt Haskell to write some of our
backend APIs and, like other projects at the company, we are using Docker for
deploying them to staging and production environments.

Working with Haskell required some re-thinking on how we do certain parts of
our workflow, but it also led to a much improved image size, with some extra
room for improvement.

![](/images/posts/haskell-containers.png)

Unlike PHP or JavaScript, Haskell is a compiled language, meaning that some of
previous approaches we were using for building Docker images did not port that
well.

For dynamic languages, we followed roughly these steps to build our images:

- Use a base image with packages for the software we need (Debian, Alpine).
- Install server software, such as Nginx, Node.js, and PHP.
- Install language tools and package managers.
- Copy application sources.
- Download dependencies.
- Final optimizations and cleanup.

This is a hybrid approach where we have both build and runtime dependencies on
the same Docker image, which is useful because image builders such as Quay.io
can automatically build the docker image for you on every commit without the
need of an additional step in the CI pipeline, and this also has the slight
advantage of having enough tooling inside a container for light debugging.

As a result, the image size is not the smallest it could be since there is a
lot of things that are not commonly used during runtime. However, the added
convenience sort of out weighs the issues of having a larger image for these
cases.

For Haskell projects, though, things are bit different, and not in a bad way:

The community has made an excellent build tool called [Stack][3]. `stack` takes
care of mostly everything related to setting up your project: Installing GHC,
pulling dependencies, building, testing, coverage reports, documentation. When
paired with Nix, it can even pull non-Haskell dependencies for reproducible
builds.

<div class="callout-quote">
Stack takes care of mostly everything related to setting up your project.
</div>

If we try to do a hybrid image approach like above using Stack, we mainly have
to do the following on a Dockerfile:

- Download and install Stack.
- Copy application sources.
- Install GHC (`stack setup`).
- Compile project (`stack build`).

This works, but it is extremely slow and the resulting images are huge
(+800MB!).

On every Docker build, `stack` would have to download and install GHC, and then
proceed to download and compile every dependency of the project, which tended
to a good 30 minutes on a [Quay.io][2] worker node.

When developing locally, you only have to go through this process every now and
then because most of it is cached in directories such as `~/.stack` and
`.stack-work`.

Looking for faster build times and smaller images, I decided to experiment with
splitting the build and runtime aspects of the project.

The build part was already setup since we were using Travis CI for running
unit and some integration tests. When compared to basic Docker image builders,
Travis CI has the clear benefit of having a cache that can be reused across
builds without too much work. This cache allowed us to keep our built
dependencies across builds, which reduced the build time to under 5 minutes.

This enable caching of Stack builds, you just need to add the work directories
to the cache section of `.travis.yml`:

```yaml
cache:
  directories:
    - "$HOME/.stack"
    - .stack-work
```

So, getting the runtime half working meant taking the resulting build from the
previous steps and building a Docker container with enough dependencies and
data files for running the application.

FPComplete has [a great article][1] out there on how to create minimal Docker
images for Haskell projects.

The main difficulty with this process is that Haskell programs are not built
statically by default, meaning that you have to identify all the libraries the
binary is linked against and include them in the final runtime image.

<div class="callout-quote">
The main difficulty with this process is that Haskell programs are not built
statically...
</div>

In order to keep things simple, I decided to stick to using a base image, which
we could use to pull in any dependencies we don't have, like `libcurl-dev`.

I initially tried to use Alpine, since its known for being one of the smallest
images out there. However, getting a Haskell program running in it was not
trivial since it requires cross-compiling GHC.

So I settled with `debian`, which is a larger image, but has almost everything
we need out of the box.

Building a Docker image on Travis CI is a fairly simple process. Pushing it to
a registry and correctly tagging it was the annoying part. After a couple of
hours of trial and error, I made a small shell script for authenticating with
the registry and pushing a tagged image matching the current git tag and
branch.

This script is called on the `after-success` step of the Travis CI build:

```bash
#!/bin/bash
set -euo pipefail
IFS=$'\n\t'

docker build -t myapp .

# If this is not a pull request, update the branch's docker tag.
if [ $TRAVIS_PULL_REQUEST = 'false' ]; then
  docker tag myapp quay.io/myorg/myapp:${TRAVIS_BRANCH/\//-} \
    && docker push quay.io/myorg/myapp:${TRAVIS_BRANCH/\//-};

  # If this commit has a tag, use on the registry too.
  if ! test -z $TRAVIS_TAG; then
    docker tag myapp quay.io/myorg/myapp:${TRAVIS_TAG} \
      && docker push quay.io/myorg/myapp:${TRAVIS_TAG};
  fi
fi
```

As a result, we now have Docker images for our Haskell projects that are about
80 MB, which is not terrible, but can definitely be improved on.

The next steps for me are investigating how to make our images even smaller by
using a smaller base image, and automate the deployment of development
and staging environments by having Travis CI notify a scheduler that a new
image has been built.

I'm including some of my scripts and an example Dockerfile on a
[GitHub Gist][4] for reference. You will most likely have to modify them to
meet your needs.

[1]: https://www.fpcomplete.com/blog/2015/05/haskell-web-server-in-5mb
[2]: https://quay.io
[3]: http://haskellstack.org/
[4]: https://gist.github.com/etcinit/d484b72de336836a956eb51b3da231ad
