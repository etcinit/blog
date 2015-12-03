---
title: Some updates
tags: laravel, laravel5, php, hhvm, pagination, ai
uuid: d52ed484-cd81-4f2f-8828-b76328f1caf3
legacy: some-updates
---

## Laravel 5 and Foundation Pagination

Laravel 5 is not out yet, but I've already began working on some projects with it. A problem of working on the edge is that many libraries have not been adapted to work with the new version yet.

I use ZURB's Foundation (an alternative to Bootstrap) for most of my projects. However, Laravel doesn't come with pagination for Foundation.

For Laravel 4 projects, I used: https://github.com/binarix/Laravel-Foundation-Pagination which worked pretty well, however, the package hasn't been updated for 5 yet.

So in the meantime I forked it and modified it so that it will work with Laravel 5, so now you can have ZURB Foundation pagination on Laravel 5 projects:

To add it to your project, add the following to your composer.json:

```json
"require": {
    "eduard44/foundation-pagination": "1.0.*"
}
```
and the following provider to your app.php:

```php
'Chromabits\FoundationPagination\FoundationPaginationServiceProvider',
```

## Snake-in-the-box problem

As described in a previous blog post, the snake-in-the-box problem was introduced in one of my classes, and as part of a class assignment I've been building algorithms that attempt to find the best snake path inside an hypercube.

I added a new algorithm to the problem solver that executes significantly faster than the previous randomized versions I've worked on. This is achieved by using a more complex data structure to keep statistics about nodes and path lengths.

You can try it out be cloning the repo and running:

```bash
php run.php snakes:guided -d 7 -i 100000
```

This will run the algorithm in dimension 7 for 100000 iterations, which should get to a best path length of around 40-44 in just a few minutes. In comparison, the pure random version was not able to get near this number even after running for a few hours.

[GitHub Project](https://github.com/etcinit/snakes)

## Vertex

Vertex is my base Docker image that I use for most of my Docker projects. It comes with most tools you need for PHP and Node.js deployment.

I've released a new build with more up-to-date packages:

- HHVM and Nginx are now pre-configured for a single PHP project on the default host. You may add more through custom config files, however, this should make it easier to start using Vertex since most people will probably only run a single server in it
- Startup scripts are now provided to launch Nginx and HHVM on port 80
- The Laravel installer is now installed through composer instead of manually downloaded
- PHP 5.5 is now included too, although it requires manual setup since HHVM is the default
- `curl` is now included too
- Fix: Install `libgmp10` manually since the HHVM package does not seem to download it automatically

### The one liner:

Get an HHVM/Nginx server running on port 80 with a single command (assuming you have Docker installed):

```
docker run -t -i -p 80:80 eduard44/vertex
```

You can also run it on different ports by modifying the `-p` argument

[GitHub Project](https://github.com/etcinit/vertex)
[Docker Hub Page](https://registry.hub.docker.com/u/eduard44/vertex/)
