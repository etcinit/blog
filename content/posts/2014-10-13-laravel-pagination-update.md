---
title: An update on Laravel 5 pagination
tags: laravel, laravel5, pagination
uuid: b27085d7-b026-4423-83bf-39571e385239
legacy: an-update-on-laravel-5-pagination
---

Laravel 5 in its current state does not have a fully working pagination. It only seems to return simple paginators (they are not aware of the total number of items). This will most likely change in the future, but if you really need pagination working on Laravel 5 right now you can try the following:

```php
<?php

namespace App\Support;

use Illuminate\Database\Eloquent\Builder;
use Illuminate\Pagination\LengthAwarePaginator;
use Illuminate\Pagination\Paginator;

/**
 * Class PaginationFactory
 *
 * @package App\Support
 */
class PaginationFactory
{
    public static function makeLengthAware(Builder $builder, $perPage = null, $columns = ['*'])
    {
        $page = Paginator::resolveCurrentPage();

        $builder->skip(($page - 1) * $perPage)->take($perPage + 1);

        $queryClone = clone ($builder->getQuery());

        $total = $queryClone->skip(0)->take($perPage + 1)->count($columns);

        return new LengthAwarePaginator($builder->get($columns), $total, $perPage, $page, [
            'path' => Paginator::resolveCurrentPath()
        ]);
    }
}
```

This will build a `LengthAwarePaginator` from a `Builder`. It seems to work fine but I'm still waiting for an official solution.
