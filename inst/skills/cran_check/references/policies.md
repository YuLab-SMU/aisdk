# CRAN Manual Verification Policies

This document details the manual checks required for CRAN submission, as the automated check cannot verify semantic quality.

## 1. Title & Description

- **Title Case**: Ensure the `Title` field in DESCRIPTION is in Title Case.
- **Informativeness**: The `Description` field must be detailed. If shorter than 2 sentences, expand it.
- **No Redundancy**: The `Description` should *not* start with "A package to...", "This package...", or repeat the package name.
- **Quotes**: Do not use quotes in the `Title` or `Description`.

## 2. Formatting Standards

- **Function Names**: Must be written as `foo()` (with parentheses, no quotes).
- **External Software**: Names of other packages or software (e.g., 'OpenSSL', 'Python') must be in single quotes ('OpenSSL').
- **References**:
    - Must be in author-year style.
    - DOI: `<doi:10.prefix/suffix>`
    - arXiv: `<doi:10.48550/arXiv.ID>`
    - ISBN: include for books.
    - URLs: enclose in angle brackets `<https://...>`

## 3. Authors & Metadata

- **Authors@R**: Ensure this field is present and lists all contributors with appropriate roles (`cre`, `aut`, `ctb`).
- **ORCID**: Check for comments: `comment = c(ORCID = "...")`.
- **ROR**: Check for organization IDs: `comment = c(ROR = "...")`.
