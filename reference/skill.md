# Skill Class

R6 class representing a skill with progressive loading capabilities. A
Skill consists of:

- Level 1: YAML frontmatter (name, description) - always loaded

- Level 2: SKILL.md body (detailed instructions) - on demand

- Level 3: R scripts (executable code) - executed by agent

## Public fields

- `name`:

  The unique name of the skill (from YAML frontmatter).

- `description`:

  A brief description of the skill (from YAML frontmatter).

- `path`:

  The directory path containing the skill files.

## Methods

### Public methods

- [`Skill$new()`](#method-Skill-new)

- [`Skill$load()`](#method-Skill-load)

- [`Skill$execute_script()`](#method-Skill-execute_script)

- [`Skill$list_scripts()`](#method-Skill-list_scripts)

- [`Skill$print()`](#method-Skill-print)

- [`Skill$clone()`](#method-Skill-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new Skill object by parsing a SKILL.md file.

#### Usage

    Skill$new(path)

#### Arguments

- `path`:

  Path to the skill directory (containing SKILL.md).

#### Returns

A new Skill object.

------------------------------------------------------------------------

### Method [`load()`](https://rdrr.io/r/base/load.html)

Load the full SKILL.md body content (Level 2).

#### Usage

    Skill$load()

#### Returns

Character string containing the skill instructions.

------------------------------------------------------------------------

### Method `execute_script()`

Execute an R script from the skill's scripts directory (Level 3). Uses
callr for safe, isolated execution.

#### Usage

    Skill$execute_script(script_name, args = list())

#### Arguments

- `script_name`:

  Name of the script file (e.g., "normalize.R").

- `args`:

  Named list of arguments to pass to the script.

#### Returns

The result from the script execution.

------------------------------------------------------------------------

### Method `list_scripts()`

List available scripts in the skill's scripts directory.

#### Usage

    Skill$list_scripts()

#### Returns

Character vector of script file names.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print a summary of the skill.

#### Usage

    Skill$print()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Skill$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
