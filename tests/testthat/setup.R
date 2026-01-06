# Test setup - runs before any tests
# See https://testthat.r-lib.org/articles/special-files.html

sftp_test_remote_dir <- function() {
  "/tmp/sftpr-tests"
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
  unlink(sftp_test_remote_dir(), recursive = TRUE)
}

# Run setup (clean first in case previous run didn't clean up properly)
sftp_cleanup()
sftp_setup()

# Register cleanup to run after all tests complete
withr::defer(sftp_cleanup(), teardown_env())

# done
