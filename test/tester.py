from time import time
from requests import post
import sys

SERVER_ADDRESS='http://localhost'
SERVER_PORT=5000

def perform_test(file: str) -> (str, list | None, float):
  with open(file, 'r') as f:
    start = time()
    result = post(f'{SERVER_ADDRESS}:{SERVER_PORT}/', data=f.read(), headers={'Content-Type': 'application/json', 'Accept': 'application/json'})
    end = time()

    return (file, result.json(), end - start)

if __name__ == '__main__':
  # Find all files in the test directory
  import os
  import glob

  files = glob.glob(os.path.join(os.path.dirname(__file__), 'tests', '*.json'))
  files.sort(key=lambda x: os.path.basename(x))

  results = []
  for (file, idx) in zip(files, range(len(files))):
    print(f"Running test {idx + 1}/{len(files)} ({os.path.basename(file)})",  file=sys.stderr)
    results.append(perform_test(file))
  
  results.sort(key=lambda x: x[0])
  
  # Print results
  for result in results:
    # Get filename without path
    filename = os.path.basename(result[0])

    print(f"({result[2]:2.2f}sec) Test {filename:50s}: {result[1]}")
