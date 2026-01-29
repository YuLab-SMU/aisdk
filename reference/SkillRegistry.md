# SkillRegistry Class

R6 class that manages a collection of skills. Provides methods to:

- Scan directories for SKILL.md files

- Cache skill metadata (Level 1)

- Retrieve skills by name

- Generate prompt sections for LLM context

## Methods

### Public methods

- [`SkillRegistry$new()`](#method-SkillRegistry-new)

- [`SkillRegistry$scan_skills()`](#method-SkillRegistry-scan_skills)

- [`SkillRegistry$get_skill()`](#method-SkillRegistry-get_skill)

- [`SkillRegistry$has_skill()`](#method-SkillRegistry-has_skill)

- [`SkillRegistry$list_skills()`](#method-SkillRegistry-list_skills)

- [`SkillRegistry$count()`](#method-SkillRegistry-count)

- [`SkillRegistry$generate_prompt_section()`](#method-SkillRegistry-generate_prompt_section)

- [`SkillRegistry$print()`](#method-SkillRegistry-print)

- [`SkillRegistry$clone()`](#method-SkillRegistry-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new SkillRegistry, optionally scanning a directory.

#### Usage

    SkillRegistry$new(path = NULL)

#### Arguments

- `path`:

  Optional path to scan for skills on creation.

#### Returns

A new SkillRegistry object.

------------------------------------------------------------------------

### Method [`scan_skills()`](https://YuLab-SMU.github.io/aisdk/reference/scan_skills.md)

Scan a directory for skill folders containing SKILL.md files.

#### Usage

    SkillRegistry$scan_skills(path, recursive = FALSE)

#### Arguments

- `path`:

  Path to the directory to scan.

- `recursive`:

  Whether to scan subdirectories. Default FALSE.

#### Returns

The registry object (invisibly), for chaining.

------------------------------------------------------------------------

### Method `get_skill()`

Get a skill by name.

#### Usage

    SkillRegistry$get_skill(name)

#### Arguments

- `name`:

  The name of the skill to retrieve.

#### Returns

The Skill object, or NULL if not found.

------------------------------------------------------------------------

### Method `has_skill()`

Check if a skill exists in the registry.

#### Usage

    SkillRegistry$has_skill(name)

#### Arguments

- `name`:

  The name of the skill to check.

#### Returns

TRUE if the skill exists, FALSE otherwise.

------------------------------------------------------------------------

### Method [`list_skills()`](https://YuLab-SMU.github.io/aisdk/reference/list_skills.md)

List all registered skills with their names and descriptions.

#### Usage

    SkillRegistry$list_skills()

#### Returns

A data.frame with columns: name, description.

------------------------------------------------------------------------

### Method `count()`

Get the number of registered skills.

#### Usage

    SkillRegistry$count()

#### Returns

Integer count of skills.

------------------------------------------------------------------------

### Method `generate_prompt_section()`

Generate a prompt section listing available skills. This can be injected
into the system prompt.

#### Usage

    SkillRegistry$generate_prompt_section()

#### Returns

Character string with formatted skill list.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print a summary of the registry.

#### Usage

    SkillRegistry$print()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    SkillRegistry$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
