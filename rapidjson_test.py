import time
import rapidjson

tic = time.perf_counter()
with open ('canada.json', 'r') as f:
    d = rapidjson.load(f)
toc = time.perf_counter()
print(f'rapidjson : {toc - tic:0.9f} seconds')
