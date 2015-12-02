---
title: Laravel 5: Custom Logs
tags: laravel, laravel5, logs, php
uuid: f9095ab9-008f-44fd-aa54-b148dcd007e0
---

Laravel 5 introduces the concept of "bootstrappers", which are classes whose sole purpose is to setup the application. This new layer of abstraction makes many of the initialization code of the application modular and portable, so if you want to replace or change how a part or the whole initialization process, you can!

<hr>

__Update:__ The behavior implemented in the fragment below is now implemented by default in the following commit [7b4e918](https://github.com/laravel/framework/commit/9dbe8f2eb5ca11f7357c6435b6ea299317cdade3). You can still use the code below to define your own custom logger

<hr>

The following is an example of a bootstrap class that replaces the regular logging service, with a logger that uses daily files (keeps one different file per day automatically):

```php
<?php

namespace App\Bootstrap;

use Illuminate\Log\Writer;
use Monolog\Logger as Monolog;
use Illuminate\Contracts\Foundation\Application;

/**
 * Class ConfigureLogging
 *
 * Setup Custom Logging
 *
 * @package App\Bootstrap
 */
class ConfigureLogging
{
    /**
     * Bootstrap the given application.
     *
     * @param  \Illuminate\Contracts\Foundation\Application  $app
     * @return void
     */
    public function bootstrap(Application $app)
    {
        $logger = new Writer(new Monolog($app->environment()), $app['events']);

        // Daily files are better for production stuff
        $logger->useDailyFiles(storage_path('/logs/myapp.log'));

        $app->instance('log', $logger);

        // Next we will bind the a Closure to resolve the PSR logger implementation
        // as this will grant us the ability to be interoperable with many other
        // libraries which are able to utilize the PSR standardized interface.
        $app->bind('Psr\Log\LoggerInterface', function ($app) {
            return $app['log']->getMonolog();
        });

        $app->bind('Illuminate\Contracts\Logging\Log', function ($app) {
            return $app['log'];
        });
    }
}
```

Once the bootstrap is defined, you can override the `$bootstrappers` field on both the Http and Console Kernels:

```php
/**
 * The bootstrap classes for the application.
 *
 * @return void
 */
protected $bootstrappers = [
        'Illuminate\Foundation\Bootstrap\DetectEnvironment',
        'Illuminate\Foundation\Bootstrap\LoadConfiguration',
        'App\Bootstrap\ConfigureLogging',
        'Illuminate\Foundation\Bootstrap\RegisterFacades',
        'Illuminate\Foundation\Bootstrap\SetRequestForConsole',
        'Illuminate\Foundation\Bootstrap\RegisterProviders',
        'Illuminate\Foundation\Bootstrap\BootProviders',
];
```

__NOTE__: Laravel 5 is under active development. This method might not work on the final release. Also, once you override the `$bootstrappers` variable you need to make sure it stays up to date with Laravel's code (at least while it is non-stable) since they could rename some of these classes.
