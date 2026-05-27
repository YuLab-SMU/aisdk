test_that("SkillStore parses GitHub repository and subdirectory refs", {
  install_path <- tempfile("aisdk-skill-store-")
  store <- SkillStore$new(install_path = install_path)
  on.exit(unlink(install_path, recursive = TRUE), add = TRUE)

  private <- store$.__enclos_env__$private

  root_ref <- private$parse_skill_ref("owner/repo")
  expect_equal(root_ref$type, "github")
  expect_equal(root_ref$owner, "owner")
  expect_equal(root_ref$repo, "repo")
  expect_null(root_ref$path)
  expect_equal(root_ref$name, "repo")

  subdir_ref <- private$parse_skill_ref("owner/repo/skills/data-cleaner")
  expect_equal(subdir_ref$type, "github")
  expect_equal(subdir_ref$owner, "owner")
  expect_equal(subdir_ref$repo, "repo")
  expect_equal(subdir_ref$path, "skills/data-cleaner")
  expect_equal(subdir_ref$name, "data-cleaner")
})

test_that("SkillStore parses GitHub tree URLs with branch and path", {
  install_path <- tempfile("aisdk-skill-store-")
  store <- SkillStore$new(install_path = install_path)
  on.exit(unlink(install_path, recursive = TRUE), add = TRUE)

  private <- store$.__enclos_env__$private

  parsed <- private$parse_skill_ref("https://github.com/owner/repo/tree/dev/skills/foo")
  expect_equal(parsed$type, "github")
  expect_equal(parsed$owner, "owner")
  expect_equal(parsed$repo, "repo")
  expect_equal(parsed$branch, "dev")
  expect_equal(parsed$path, "skills/foo")
  expect_equal(parsed$name, "foo")
})

test_that("SkillStore parses public GitHub repository refs without auth ownership", {
  install_path <- tempfile("aisdk-skill-store-")
  store <- SkillStore$new(install_path = install_path)
  on.exit(unlink(install_path, recursive = TRUE), add = TRUE)

  private <- store$.__enclos_env__$private

  root_ref <- private$parse_skill_ref("https://github.com/posit-dev/skills")
  expect_equal(root_ref$type, "github")
  expect_equal(root_ref$owner, "posit-dev")
  expect_equal(root_ref$repo, "skills")
  expect_null(root_ref$path)
  expect_null(root_ref$branch)
  expect_equal(root_ref$name, "skills")

  path_ref <- private$parse_skill_ref("posit-dev/skills/data-wrangling")
  expect_equal(path_ref$type, "github")
  expect_equal(path_ref$owner, "posit-dev")
  expect_equal(path_ref$repo, "skills")
  expect_equal(path_ref$path, "data-wrangling")
  expect_equal(path_ref$name, "data-wrangling")
})

test_that("SkillStore installs a subdirectory from an archive", {
  skip_if(Sys.which("zip") == "", "zip command is not available")

  install_path <- tempfile("aisdk-skill-store-")
  store <- SkillStore$new(install_path = install_path)
  on.exit(unlink(install_path, recursive = TRUE), add = TRUE)

  archive_root <- tempfile("aisdk-skill-archive-")
  dir.create(file.path(archive_root, "repo-main", "skills", "foo"), recursive = TRUE)
  on.exit(unlink(archive_root, recursive = TRUE), add = TRUE)

  writeLines(
    c("---", "name: foo", "description: Foo skill", "---", "", "# Foo"),
    file.path(archive_root, "repo-main", "skills", "foo", "SKILL.md")
  )
  writeLines(
    c("---", "name: root", "description: Root skill", "---", "", "# Root"),
    file.path(archive_root, "repo-main", "SKILL.md")
  )

  archive_path <- tempfile(fileext = ".zip")
  old_wd <- setwd(archive_root)
  on.exit(setwd(old_wd), add = TRUE)
  utils::zip(archive_path, files = "repo-main", flags = "-r9Xq")

  dest_dir <- file.path(install_path, "foo")
  private <- store$.__enclos_env__$private
  private$download_and_extract(
    paste0("file://", normalizePath(archive_path, winslash = "/", mustWork = TRUE)),
    dest_dir,
    "repo",
    subdir = "skills/foo"
  )

  expect_true(file.exists(file.path(dest_dir, "SKILL.md")))
  expect_false(file.exists(file.path(dest_dir, "skills", "foo", "SKILL.md")))
})

test_that("skill_to_store_record exports store-ready metadata", {
  skill_dir <- tempfile("aisdk-skill-")
  dir.create(file.path(skill_dir, "scripts"), recursive = TRUE)
  dir.create(file.path(skill_dir, "references"), recursive = TRUE)
  on.exit(unlink(skill_dir, recursive = TRUE), add = TRUE)

  writeLines(c(
    "---",
    "name: sample-skill",
    "description: Sample skill",
    "version: 1.2.3",
    "author:",
    "  name: Test Author",
    "repository:",
    "  type: github",
    "  owner: example",
    "  repo: store-repo",
    "  branch: main",
    "capabilities:",
    "  - analysis",
    "dependencies:",
    "  - dplyr",
    "system_requirements:",
    "  - python >= 3.11",
    "entry:",
    "  main: SKILL.md",
    "  scripts: scripts/",
    "---",
    "",
    "# Sample skill",
    "",
    "Use this skill for structured analysis."
  ), file.path(skill_dir, "SKILL.md"))

  writeLines(c(
    "#' Summarize the input",
    "summarize <- function() {",
    "  args$input",
    "}"
  ), file.path(skill_dir, "scripts", "summarize.R"))

  writeLines("Reference guide", file.path(skill_dir, "references", "guide.md"))

  skill <- Skill$new(skill_dir)
  record <- skill_to_store_record(skill)

  expect_equal(record$id, "example/store-repo")
  expect_equal(record$name, "sample-skill")
  expect_equal(record$version, "1.2.3")
  expect_true("analysis" %in% record$capabilities)
  expect_true("dplyr" %in% record$dependencies)
  expect_true("python >= 3.11" %in% record$system_requirements)
  expect_equal(record$repository$owner, "example")
  expect_true(nzchar(record$readme))
  expect_true(nzchar(record$code))
  expect_true(nzchar(record$readme_url))
  expect_equal(record$tools[[1]]$name, "summarize")
  expect_true("input" %in% names(record$tools[[1]]$parameters))
  expect_true("scripts/summarize.R" %in% names(record$scripts))
  expect_true("references/guide.md" %in% names(record$references))
})

test_that("record_skill_iteration writes a local iteration log", {
  skill_dir <- tempfile("aisdk-skill-")
  dir.create(skill_dir, recursive = TRUE)
  on.exit(unlink(skill_dir, recursive = TRUE), add = TRUE)

  writeLines(c(
    "---",
    "name: iteration-skill",
    "description: Iteration skill",
    "version: 0.1.0",
    "---",
    "",
    "# Iteration skill"
  ), file.path(skill_dir, "SKILL.md"))

  skill <- Skill$new(skill_dir)
  out_file <- record_skill_iteration(
    skill,
    iteration = list(
      step = "revise-description",
      note = "Captured a fresh iteration"
    )
  )

  expect_true(file.exists(out_file))
  payload <- jsonlite::read_json(out_file, simplifyVector = FALSE)
  expect_equal(payload$skill$name, "iteration-skill")
  expect_equal(payload$iteration$step, "revise-description")
  expect_equal(payload$iteration$note, "Captured a fresh iteration")
  expect_true(is.list(payload$record))
})

test_that("sync_skill publishes and records iteration", {
  skill_dir <- tempfile("aisdk-skill-")
  dir.create(skill_dir, recursive = TRUE)
  on.exit(unlink(skill_dir, recursive = TRUE), add = TRUE)

  writeLines(c(
    "---",
    "name: sync-skill",
    "description: Sync skill",
    "version: 0.1.0",
    "---",
    "",
    "# Sync skill"
  ), file.path(skill_dir, "SKILL.md"))

  calls <- list()
  publish_stub <- function(...) {
    calls$publish <<- list(...)
    list(ok = TRUE)
  }
  record_stub <- function(...) {
    calls$record <<- list(...)
    tempfile("sync-skill-iteration-")
  }

  result <- sync_skill(
    skill = skill_dir,
    github_user = "example",
    github_token = "token",
    repo_name = "skill-store",
    iteration = list(step = "sync"),
    publish_fun = publish_stub,
    record_fun = record_stub
  )

  expect_true(is.list(result$publish))
  expect_equal(calls$publish$github_user, "example")
  expect_equal(calls$record$iteration$step, "sync")
  expect_true(nzchar(result$iteration_path))
})
