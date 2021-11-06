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
fpm run read_file_test
fpm run fson_test

python json_test.py
python ujson_test.py
python rapidjson_test.py
```

## Sample results:

To parse the `canada.json` file:

First, just the time to read the entire file into a `character(len=:),allocatable` string:
```
 read file to a string :   0.0003  seconds
```

### Fortran libs
```
 fson         : 1.0160  seconds
 rojff        : 0.3667  seconds
 json_fortran : 0.1161  seconds
```

### Python libs
```
 rapidjson    : 0.0442  seconds
 json         : 0.0326  seconds
 ujson        : 0.0205  seconds
```


## See also
 * [JSON](https://degenerateconic.com/json.html) [degenerateconic.com]