# Test a Newly Created Skill

Verifies a skill by running a test query against it in a sandboxed
session.

## Usage

``` r
test_new_skill(skill_name, test_query, registry, model)
```

## Arguments

- skill_name:

  Name of the skill to test (must be in the registry).

- test_query:

  A natural language query to test the skill (e.g., "Use hello_world to
  say hi").

- registry:

  The skill registry object.

- model:

  A model object to use for the test agent.

## Value

A list containing `success` (boolean) and `result` (string).
