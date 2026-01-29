# Skill System

The Skill System allows you to package R scripts and instructions into
reusable units that agents can load dynamically.

## Structure of a Skill

A skill is a directory containing: - `SKILL.md`: Metadata (YAML header)
and instructions. - `scripts/`: R scripts that can be executed.

**Example File: `greeting/SKILL.md`**

``` markdown
---
name: greeting_skill
description: Generates greetings
---
# Instructions
Use the `greet.R` script to greet users.
```

**Example File: `greeting/scripts/greet.R`**

``` r
# Expects args$name
sprintf("Hello, %s!", args$name)
```

## Using Skills

1.  **Create Registry**: Point to your skills folder.

``` r
library(aisdk)
registry <- create_skill_registry("./my_skills")
```

2.  **Generate with Skills**: Pass the registry to `generate_text`.

``` r
# The model will see the skill descriptions and can decide to load them
result <- generate_text(
  model = "openai:gpt-4o",
  prompt = "Use the greeting skill to say hi to Alice",
  skills = registry,
  max_steps = 3 
)

# Under the hood:
# 1. Model sees "greeting_skill" available.
# 2. Model calls "load_skill('greeting_skill')".
# 3. Model sees "greet.R" script.
# 4. Model calls "execute_skill_script('greet.R', name='Alice')".
# 5. Model returns result.
```
