Dear CRAN team,

  thank you for the review and the detailed guidance.

  I have addressed all reported issues in the resubmitted version:

  - The redundant “in R” was removed from the package Title.
  - The example for the unexported internal function `repair_tool_call()` was removed.
  - File-writing defaults were revised so that package functions no longer write by default to the user home directory, package directory, or `getwd()`.
  Examples and vignettes now use temporary directories where appropriate.
  - Package code was updated to avoid modifying `.GlobalEnv`, and uses of `<<-` in package functions were removed.

  The package documentation was regenerated and the package was rebuilt before resubmission.

  Best regards,
  Yonghe Xia