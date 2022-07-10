import uuid
import random
import json

"""
A python script to generate dummy data for the project.
"""

MIN_LIST_ITEMS = 20

predicate_name = "room"
variables = [
    # id
    {
        "vartype": "string",
    },
    # type
    {
        "vartype": "choice",
        "choices": ["Aud", "Lab"]
    },
    # capacity
    {
        "vartype": "integer",
        "max": 150,
        "min": 50
    },
    # availability
    {
        "vartype": "list",
        "min_value": 0,
        "max_value": 69,
    }
]

def create_data(datum):
    if datum["vartype"] == 'string':
        return str(uuid.uuid4())
    elif datum["vartype"] == 'integer':
        return random.randint(datum["min"], datum["max"])
    elif datum["vartype"] == 'choice':
        return random.choice(datum["choices"])
    elif datum["vartype"] == 'list':
        timeslots = random.sample(
            range(datum["min_value"], datum["max_value"]), 
            random.randint(datum["min_value"] + MIN_LIST_ITEMS, datum["max_value"] - MIN_LIST_ITEMS)
        )
        timeslots.sort()
        return timeslots
    else:
        return 0;

DATA_COUNT = 10

def main():

    facts = []
    for _ in range(DATA_COUNT):
        # Create a new data object
        fact = predicate_name + "("
        for variable in variables:
            fact += json.dumps(create_data(variable)) + ","
        fact = fact[:-1] + ")"
        
        # Add the fact to the list
        facts.append(fact)

    # Write the facts to a file
    with open("data/dummy_data.txt", "w") as f:
        for fact in facts:
            f.write(fact + "\n")   


if __name__ == '__main__':
    main()