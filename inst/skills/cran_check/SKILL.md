---
name: cran_check
description: Perform a comprehensive CRAN pre-submission check on an R package. Use this skill when the user asks to "check the package", "verify CRAN compliance", or "prepare for submission".
---

# CRAN Check Protocol

This skill helps you verify if an R package is ready for CRAN submission. It combines automated checks with manual policy verification.

## 1. Automated Verification

Run the official R check using the standardized script. This is non-negotiable.

**Action:** Execute `scripts/run_check.R`

Arguments:
- `check_dir` (optional): Directory to store check outputs. Defaults to "check".

**Analyze the Output:**
-   **ERROR**: Immediate failure. Fix required.
-   **WARNING**: Immediate failure. Fix required.
-   **NOTE**: Must be explained or fixed.

## 2. Manual Verification

CRAN has strict policies about the `DESCRIPTION` file and metadata that automated tools often miss.

**Action:**
1.  Read the policy guide: `references/policies.md` using `read_skill_resource`.
2.  Verify the package's `DESCRIPTION` file against the checklist in `policies.md`.

## 3. Reporting

If *any* check (automated or manual) fails:
1.  List the specific failure.
2.  Provide the exact fix based on the policy.
3.  Do NOT proceed until fixed.
