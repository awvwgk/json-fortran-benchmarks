# json-fortran-benchmarks
Benchmarks for JSON Fortran parsers. Also comparison to Python.

## To compile

```
fpm build --profile release
```

## To run tests

```
# fun all the fortran tests:
fpm run --profile release 

# run the python tests:
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
              json_fortran C:   0.0447  seconds
               json_fortran :   0.0826  seconds
                      tomlf :   0.1079  seconds
                      jsonf :   0.2109  seconds
                      rojff :   0.2829  seconds
                    jonquil :   0.4208  seconds
                       fson :   0.9652  seconds
```

Note that `json_fortran C` is just json-fortran but using the `string_to_real_mode=2` option, which uses the string to real functions from the C stdlib.

### Python libs
```
json      : 0.038878583 seconds
rapidjson : 0.048005750 seconds
ujson     : 0.016822333 seconds
```

## See also
 * [JSON](https://degenerateconic.com/json.html) [degenerateconic.com]