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
fpm run --profile release jonquil_test 
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
      read file to a string :   0.0003  seconds
```

### Fortran libs
```
               json_fortran :   0.0813  seconds
                      tomlf :   0.1085  seconds
                 jsonf test :   0.2074  seconds
                      rojff :   0.3028  seconds
                    jonquil :   0.4208  seconds
                       fson :   0.9588  seconds
```

### Python libs
```
json      : 0.038878583 seconds
rapidjson : 0.048005750 seconds
ujson     : 0.016822333 seconds
```

## See also
 * [JSON](https://degenerateconic.com/json.html) [degenerateconic.com]