# Skill Store Class

R6 class for managing the global skill store, including installation,
updates, and discovery of skills.

## Public fields

- `registry_url`:

  URL of the skill registry.

- `install_path`:

  Local path for installed skills.

- `installed`:

  List of installed skills.

## Methods

### Public methods

- [`SkillStore$new()`](#method-SkillStore-new)

- [`SkillStore$install()`](#method-SkillStore-install)

- [`SkillStore$uninstall()`](#method-SkillStore-uninstall)

- [`SkillStore$get()`](#method-SkillStore-get)

- [`SkillStore$list_installed()`](#method-SkillStore-list_installed)

- [`SkillStore$search()`](#method-SkillStore-search)

- [`SkillStore$update_all()`](#method-SkillStore-update_all)

- [`SkillStore$validate()`](#method-SkillStore-validate)

- [`SkillStore$print()`](#method-SkillStore-print)

- [`SkillStore$clone()`](#method-SkillStore-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new SkillStore instance.

#### Usage

    SkillStore$new(registry_url = NULL, install_path = NULL)

#### Arguments

- `registry_url`:

  URL of the skill registry.

- `install_path`:

  Local installation path.

#### Returns

A new SkillStore object.

------------------------------------------------------------------------

### Method `install()`

Install a skill from the registry or a GitHub repository.

#### Usage

    SkillStore$install(skill_ref, version = NULL, force = FALSE)

#### Arguments

- `skill_ref`:

  Skill reference (e.g., "username/skillname" or registry name).

- `version`:

  Optional specific version to install.

- `force`:

  Force reinstallation even if already installed.

#### Returns

The installed Skill object.

------------------------------------------------------------------------

### Method `uninstall()`

Uninstall a skill.

#### Usage

    SkillStore$uninstall(name)

#### Arguments

- `name`:

  Skill name.

#### Returns

Self (invisibly).

------------------------------------------------------------------------

### Method [`get()`](https://rdrr.io/r/base/get.html)

Get an installed skill.

#### Usage

    SkillStore$get(name)

#### Arguments

- `name`:

  Skill name.

#### Returns

A Skill object or NULL.

------------------------------------------------------------------------

### Method `list_installed()`

List installed skills.

#### Usage

    SkillStore$list_installed()

#### Returns

A data frame of installed skills.

------------------------------------------------------------------------

### Method [`search()`](https://rdrr.io/r/base/search.html)

Search the registry for skills.

#### Usage

    SkillStore$search(query = NULL, capability = NULL)

#### Arguments

- `query`:

  Search query.

- `capability`:

  Filter by capability.

#### Returns

A data frame of matching skills.

------------------------------------------------------------------------

### Method `update_all()`

Update all installed skills to latest versions.

#### Usage

    SkillStore$update_all()

#### Returns

Self (invisibly).

------------------------------------------------------------------------

### Method `validate()`

Validate a skill.yaml manifest.

#### Usage

    SkillStore$validate(path)

#### Arguments

- `path`:

  Path to skill directory or skill.yaml file.

#### Returns

A list with validation results.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print method for SkillStore.

#### Usage

    SkillStore$print()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    SkillStore$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
