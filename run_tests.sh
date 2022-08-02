#!/bin/sh

fpm build --profile release

echo 'python tests'

python ./json_test.py
python ./ujson_test.py
python ./rapidjson_test.py  

echo 'fortran tests'

fpm run --profile release read_file_test

# fpm run --profile release jsonff_test         # crashes?
fpm run --profile release fson_test
fpm run --profile release tomlf_test
fpm run --profile release rojff_test
fpm run --profile release json_fortran_test  
