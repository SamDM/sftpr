# Test setup - runs before any tests
# See https://testthat.r-lib.org/articles/special-files.html

sftp_test_remote_dir <- function() {
  "/tmp/sftpr-tests"
}

sftp_test_url <- function(filename) {
  sprintf("sftp://vscode@localhost:2222%s/%s", sftp_test_remote_dir(), filename)
}

sftp_setup <- function(port = 2222) {
  remote_dir <- sftp_test_remote_dir()

  # Create test directory and health check file
  dir.create(remote_dir, recursive = TRUE, showWarnings = FALSE)
  writeLines("hello SFTP", file.path(remote_dir, "hello_sftp.txt"))

  # Health check: fetch the file via SFTP and verify contents
  local_check <- tempfile("sftp_health_", fileext = ".txt")
  on.exit(unlink(local_check), add = TRUE)

  result <- system2(
    "sftp",
    c("-o", "BatchMode=yes",
      "-P", port,
      sprintf("vscode@localhost:%s/hello_sftp.txt", remote_dir),
      local_check),
    stdout = FALSE,
    stderr = FALSE
  )

  if (result != 0) {
    stop("SFTP health check failed: could not connect to server")
  }

  content <- readLines(local_check, warn = FALSE)
  if (!identical(content, "hello SFTP")) {
    stop("SFTP health check failed: content mismatch")
  }

  invisible(TRUE)
}

sftp_cleanup <- function() {
  test_dir <- sftp_test_remote_dir()

  # Nothing to do if test dir doesn't exist
  if (!dir.exists(test_dir)) {
    return(invisible())
  }

  # Find existing archived test dirs (sftpr-tests-N)
  existing <- list.files("/tmp", pattern = "^sftpr-tests-[0-9]+$", full.names = TRUE)
  existing_nums <- as.integer(sub(".*sftpr-tests-", "", existing))

  # Determine next number
  next_num <- if (length(existing_nums) == 0) 1 else max(existing_nums) + 1

  # Rename current test dir to archived name
  file.rename(test_dir, sprintf("/tmp/sftpr-tests-%d", next_num))

  # If more than 3 archived dirs, remove all but the most recent 3
  if (length(existing) >= 3) {
    sorted_dirs <- existing[order(existing_nums, decreasing = TRUE)]
    dirs_to_remove <- sorted_dirs[4:length(sorted_dirs)]
    unlink(dirs_to_remove, recursive = TRUE)
  }

  invisible()
}

# Run setup (clean first in case previous run didn't clean up properly)
sftp_cleanup()
sftp_setup()

# Register cleanup to run after all tests complete
withr::defer(sftp_cleanup(), teardown_env())
