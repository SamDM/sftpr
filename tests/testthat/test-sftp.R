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
