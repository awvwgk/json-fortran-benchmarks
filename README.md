# json-fortran-benchmarks
Benchmarks for JSON Fortran parsers. Also comparison to Python.

## To compile

```
fpm build --profile Release
```

## To run tests

```
fpm run rojff_test         
fpm run json_fortran_test
python json_test.py
python ujson_test.py
```

## Sample results:

```
 rojff        : 0.686999977  seconds
 json_fortran : 0.210999995  seconds
 json         : 0.0391       seconds
 ujson        : 0.0248       seconds
```