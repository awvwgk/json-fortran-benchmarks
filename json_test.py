import time
import json

tic = time.perf_counter()
with open ('canada.json', 'r') as f:
    d = json.load(f)
toc = time.perf_counter()
print(f"json : {toc - tic:0.4f} seconds")
