import time
import ujson

tic = time.perf_counter()
with open ('canada.json', 'r') as f:
    d = ujson.load(f)
toc = time.perf_counter()
print(f'ujson : {toc - tic:0.9f} seconds')


