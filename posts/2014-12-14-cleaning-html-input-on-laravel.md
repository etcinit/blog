---
title: Cleaning HTML input on Laravel 5
tags: laravel5, php, purifier, html
uuid: ad9d55d9-ece0-4dc5-be54-15672e843468
---

## Introduction

I've been using Laravel 5 for a few months now. One of the main changes in the new framework is the extesive use of dependency injection on classes and controllers.

Many packages made for Laravel 4 have two main incompatibilities on Laravel 5:

- Version `4.x.x` is hardcoded in the `composer.json` file
- Most services are available as Facades, instead of using DI (which won't prevent your from suing the library but it does defeat the purpose of DI)

Dependency injection is much nicer since PHP IDEs are able to perform autocompletion and type hinting.

### Porting Purifier to Laravel 5

A package that I use frequently on Laravel 4 projects is the [HTMLPurifier Service Provider](https://github.com/mewebstudio/Purifier), which simplifies setup for using the PHP HTMLPurifier library.

HTMLPurifier allows you to clean input before it is store or displayed on your application. Though it also has a few nice tricks like converting lines into `<p>...</p>` elements or only allowing certain tags and attributes.

The problem: this library hasn't been updated in a while and it's not possible to use it on Laravel 5 due to the `composer.json` issue.

So I decided to rewrite parts of it and port it into Laravel 5

> **WARNING:** Laravel 5 is still under active development. This library or the framework itself might stop working at any point. I'll try to keep it updated.

Once you get the library setup on your project, using the purfifier is as simple as defining the depency on oyur class constructor and using the `clean` method:

```php
// ...
$cleanInput = $this->purifier->clean($dirtyInput);
```

## Setting it up on your project

### Step 1: Add Composer dependency

The first step is to add the dependency on your project's `composer.json`:

```json
{
    "require": {
        "laravel/framework": "5.0.*",
        "chromabits/purifier": "dev-master"
    },
    "minimum-stability": "dev"
}
```

and run `composer update`.

Alternatively, you can run: `composer require chromabits/purifier dev-master`

### Step 2: Use service provider

Then, load the service into your application by adding into your `config/app.php`:

```php
return [
	// ...
	'providers' => [
		// ...
		'Chromabits\Purifier\PurifierServiceProvider'
	]
];
```

### Step 3: Use within a controller

Using the service in your controller or class can be done by requiring the "Contract" (aka interface) on the constructor:

```php
<?php

namespace Http\Controllers;

use Chromabits\Purifier\Constracts\Purifier;
use HTMLPurifier_Config;
use Illuminate\Http\Request;

/**
 * Class IndexController
 *
 * @package App\Http\Controllers;
 */
class IndexController {
    /**
     * @var Purifier
     */
    protected $purifier;

    /**
     * Construct an instance of MyClass
     *
     * @param Purifier $purifier
     */
    public function __construct(Purifier $purifier) {
        // Inject dependencies
        $this->purifier = $purifier;
    }

    /**
     * Get index page
     *
     * @param Request $request
     */
    public function getIndex(Request $request)
    {
        return $this->purifier->clean($request->input('first_name'));
    }
}
```
## Source Code

The source code is available on [GitHub](https://github.com/etcinit/purifier) and the package is available on [Packagist](https://packagist.org/packages/chromabits/purifier)
