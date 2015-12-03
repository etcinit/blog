---
title: Facade-less migrations in Laravel 5
tags: laravel5, migrations
uuid: 8135efaf-3da3-4fd8-9efb-24286ccac2bc
legacy: facade-less-migration-in-laravel-5
author: Eduardo Trujillo
---

Found a quick way of avoiding using the Schema facade for migrations in Laravel:

``` {#cadvisor .php .numberLines startFrom="0"}
<?php

namespace App\Database;

use Illuminate\Database\Migrations\Migration;
use Illuminate\Database\Query\Builder;

/**
 * Class BaseMigration
 *
 * Base migration class
 *
 * @package App\Database
 */
abstract class BaseMigration extends Migration
{
    /**
     * @var Builder
     */
    protected $builder;

    /**
     * Construct an instance of a BaseMigration
     */
    public function __construct()
    {
        $this->builder = app('db')
            ->connection($this->connection)
            ->getSchemaBuilder();
    }
}
```

Why? This should help with the type hinting provided by some IDEs such as
PHPStorm.

To use this class, extend it from every migration you make and replace any
mention of `Schema::` with just `$this->builder->`
