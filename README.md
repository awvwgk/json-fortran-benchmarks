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

Library | Runtime (sec)
--- | ---
[json-fortran](https://github.com/jacobwilliams/json-fortran.git) C |   0.0447
[json-fortran](https://github.com/jacobwilliams/json-fortran.git)  |   0.0826
[toml-f](https://github.com/toml-f/toml-f.git)  |   0.1079
[jsonf](https://github.com/JeffIrwin/jsonf.git)  |   0.2109
[rojff](https://gitlab.com/everythingfunctional/rojff.git)  |   0.2829
[jonquil](https://github.com/toml-f/jonquil.git)  |   0.4208
[fson](https://github.com/josephalevin/fson)  |   0.9652

Note that `json_fortran C` is just json-fortran but using the `string_to_real_mode=2` option, which uses the string to real functions from the C stdlib.

### Python libs
Library | Runtime (sec)
--- | ---
json      | 0.038878583 seconds
rapidjson | 0.048005750 seconds
ujson     | 0.016822333 seconds

## See also
 * [JSON](https://degenerateconic.com/json.html) [degenerateconic.com]