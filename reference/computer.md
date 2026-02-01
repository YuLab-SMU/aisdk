# Computer Class

R6 class providing computer abstraction with atomic tools for file
operations, bash execution, and R code execution.

## Public fields

- `working_dir`:

  Current working directory

- `sandbox_mode`:

  Sandbox mode: "strict", "permissive", or "none"

- `execution_log`:

  Log of executed commands

## Methods

### Public methods

- [`Computer$new()`](#method-Computer-new)

- [`Computer$bash()`](#method-Computer-bash)

- [`Computer$read_file()`](#method-Computer-read_file)

- [`Computer$write_file()`](#method-Computer-write_file)

- [`Computer$execute_r_code()`](#method-Computer-execute_r_code)

- [`Computer$get_log()`](#method-Computer-get_log)

- [`Computer$clear_log()`](#method-Computer-clear_log)

- [`Computer$clone()`](#method-Computer-clone)

------------------------------------------------------------------------

### Method `new()`

Initialize computer abstraction

#### Usage

    Computer$new(working_dir = getwd(), sandbox_mode = "permissive")

#### Arguments

- `working_dir`:

  Working directory (default: current directory)

- `sandbox_mode`:

  Sandbox mode: "strict", "permissive", or "none"

------------------------------------------------------------------------

### Method `bash()`

Execute bash command

#### Usage

    Computer$bash(command, timeout_ms = 30000, capture_output = TRUE)

#### Arguments

- `command`:

  Bash command to execute

- `timeout_ms`:

  Timeout in milliseconds (default: 30000)

- `capture_output`:

  Whether to capture output (default: TRUE)

#### Returns

List with stdout, stderr, exit_code

------------------------------------------------------------------------

### Method `read_file()`

Read file contents

#### Usage

    Computer$read_file(path, encoding = "UTF-8")

#### Arguments

- `path`:

  File path (relative to working_dir or absolute)

- `encoding`:

  File encoding (default: "UTF-8")

#### Returns

File contents as character string

------------------------------------------------------------------------

### Method `write_file()`

Write file contents

#### Usage

    Computer$write_file(path, content, encoding = "UTF-8")

#### Arguments

- `path`:

  File path (relative to working_dir or absolute)

- `content`:

  Content to write

- `encoding`:

  File encoding (default: "UTF-8")

#### Returns

Success status

------------------------------------------------------------------------

### Method `execute_r_code()`

Execute R code

#### Usage

    Computer$execute_r_code(code, timeout_ms = 30000, capture_output = TRUE)

#### Arguments

- `code`:

  R code to execute

- `timeout_ms`:

  Timeout in milliseconds (default: 30000)

- `capture_output`:

  Whether to capture output (default: TRUE)

#### Returns

List with result, output, error

------------------------------------------------------------------------

### Method `get_log()`

Get execution log

#### Usage

    Computer$get_log()

#### Returns

List of logged executions

------------------------------------------------------------------------

### Method `clear_log()`

Clear execution log Log execution Resolve path (relative to working_dir
or absolute) Check bash command for sandbox violations Check write path
for sandbox violations Check R code for sandbox violations

#### Usage

    Computer$clear_log()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Computer$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
