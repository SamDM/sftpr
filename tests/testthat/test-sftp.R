test_that("sftp_put and sftp_get round-trip works", {
  # Create a local file with test content
  local_file <- tempfile("test_", fileext = ".txt")
  on.exit(unlink(local_file), add = TRUE)
  writeLines("hello world", local_file)

  # Upload to SFTP server
  remote_url <- sprintf(
    "sftp://vscode@localhost:2222%s/test_file.txt",
    sftp_test_remote_dir()
  )
  sftp_put(local_file, remote_url)

  # Download from SFTP server
  downloaded_file <- tempfile("downloaded_", fileext = ".txt")
  on.exit(unlink(downloaded_file), add = TRUE)
  sftp_get(remote_url, downloaded_file)

  # Verify content matches
  expect_equal(readLines(downloaded_file), "hello world")
})

test_that("sftp_exists returns TRUE for existing files", {
  # Create and upload a test file
  local_file <- tempfile("test_exists_", fileext = ".txt")
  on.exit(unlink(local_file), add = TRUE)
  writeLines("test content", local_file)

  remote_url <- sprintf(
    "sftp://vscode@localhost:2222%s/exists_test.txt",
    sftp_test_remote_dir()
  )
  sftp_put(local_file, remote_url)

  # File should exist

  expect_true(sftp_exists(remote_url))
})

test_that("sftp_exists returns FALSE for non-existent files", {
  remote_url <- sprintf(
    "sftp://vscode@localhost:2222%s/nonexistent_file.txt",
    sftp_test_remote_dir()
  )

  expect_false(sftp_exists(remote_url))
})
