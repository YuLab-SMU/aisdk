# Create a SkillArchitect Agent

Creates an advanced agent specialized in creating, testing, and refining
new skills. It follows a rigorous "Ingest -\> Design -\> Implement -\>
Verify" workflow.

## Usage

``` r
create_skill_architect_agent(
  name = "SkillArchitect",
  registry = NULL,
  model = NULL
)
```

## Arguments

- name:

  Agent name. Default "SkillArchitect".

- registry:

  Optional SkillRegistry object (defaults to creating one from
  inst/skills).

- model:

  The model object to use for verification (spawning a tester agent).

## Value

An Agent object configured for skill architecture.
