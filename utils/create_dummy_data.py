import uuid
import random
import json
from sys import argv

"""
A python script to generate dummy data for the project.
"""

room_pred_name = "room"
room_variables = [
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

# ==============================================================================

professor_pred_name = "professor"
professor_variables = [

    # id
    {
        "vartype": "string",
    },
    # availability
    {
        "vartype": "list",
        "min_value": 0,
        "max_value": 69,
    }
]

# ==============================================================================

group_pred_name = "group"
group_variables = [
    # id
    {
        "vartype": "string",
    },
    # memberCount
    {
        "vartype": "integer",
        "max": 150,
        "min": 25
    },
    # overlapping
    {
        "vartype": "empty_list",

    }
]

# ==============================================================================

facts_dict = {
    "room": room_variables,
    "professor": professor_variables,
    "group": group_variables
}


MIN_LIST_ITEMS = 20

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
    elif datum["vartype"] == 'empty_list':
        return []
    else:
        return 0;

def main(predicate_name, variables, facts_count):

    facts = []
    for _ in range(facts_count):
        # Create a new data object
        fact = predicate_name + "("
        for variable in variables:
            fact += json.dumps(create_data(variable)) + ","
        fact = fact[:-1] + ")."
        
        # Add the fact to the list
        facts.append(fact)

    # Write the facts to a file
    with open("data/dummy_data.txt", "w") as f:
        for fact in facts:
            f.write(fact + "\n")   


if __name__ == '__main__':
    if len(argv) != 3:
        print("Usage: create_dummy_data.py <predicate_name> <facts_count>")
        exit(1)
    
    if argv[1] not in facts_dict:
        print("Predicate not found")
        exit(1)
    
    predicate_name = argv[1]
    variables = facts_dict[predicate_name]
    facts_count = int(argv[2])
    main(predicate_name, variables, facts_count)