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
python json_test.py
python ujson_test.py
```

## Sample results:

Just the time to read the entire file into a `character(len=:),allocatable` string:
```
 read file to a string :   0.0003  seconds
```

To parse the `canada.json` file:
```
 rojff        : 0.6642  seconds
 json_fortran : 0.1925  seconds
 json         : 0.0326  seconds
 ujson        : 0.0205  seconds
```
