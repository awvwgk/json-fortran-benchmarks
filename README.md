# json-fortran-benchmarks
Benchmarks for JSON Fortran parsers. Also comparison to Python.

## To compile

```
fpm build --profile release
```

## To run tests

```
fpm run --profile release read_file_test
fpm run --profile release fson_test
fpm run --profile release tomlf_test
fpm run --profile release rojff_test         
fpm run --profile release json_fortran_test

python json_test.py
python ujson_test.py
python rapidjson_test.py
```

## Sample results:

The benchmarks were run on an M1 MacBook Pro.

To parse the `canada.json` file:

First, just the time to read the entire file into a `character(len=:),allocatable` string:
```
      read file to a string :   0.0004  seconds
```

### Fortran libs
```
               json_fortran :   0.1162  seconds
                      tomlf :   0.1352  seconds
                      rojff :   0.2367  seconds
                       fson :   0.8735  seconds
```

### Python libs
```
json      : 0.022102208 seconds
ujson     : 0.011403209 seconds
rapidjson : 0.032243625 seconds
```

## See also
 * [JSON](https://degenerateconic.com/json.html) [degenerateconic.com]