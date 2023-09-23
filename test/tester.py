from time import time
from requests import post


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

  results = []
  for file in files:
    results.append(perform_test(file))
  
  results.sort(key=lambda x: x[0])
  
  # Print results
  for result in results:
    # Get filename without path
    filename = os.path.basename(result[0])

    print(f"({result[2]:2.2f}sec) Test {filename:50s}: {result[1]}")
