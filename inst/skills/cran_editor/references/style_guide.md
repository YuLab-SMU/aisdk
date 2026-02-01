# CRAN Editorial Standards

The `DESCRIPTION` file is the face of the package. It must meet these strict typographic standards.

## 1. Title & Description

- **Title Case**: The `Title` field must be in Title Case (e.g., "A Package for Data Analysis" not "a package for data analysis").
- **Informativeness**: The `Description` field must be detailed and consist of multiple sentences. One-liners are rejected.
- **No Redundancy**: The `Description` must **not** start with "A package to...", "This package...", or simply repeat the package name.
- **Quotes**: Do not use quotes in the `Title` or `Description`.

## 2. Formatting Details

- **Function Names**: Must be written as `foo()` (with parentheses, no quotes).
- **External Software**: Names of other packages or software (e.g., 'Python', 'OpenSSL') must be in single quotes (e.g., 'Python').
- **References**:
    - Must use author-year style.
    - DOI: `<doi:10.prefix/suffix>`
    - arXiv: `<doi:10.48550/arXiv.ID>`
    - URL: `<https://...>` (in angle brackets)

## 3. Metadata

- **Authors@R**: Must use the `person()` function with correct roles (`"cre"`, `"aut"`, etc.).
- **ORCID**: Encouraged via `comment = c(ORCID = "...")`.
