test_that("sftp_put and sftp_get round-trip works", {
  local_file <- withr::local_tempfile(fileext = ".txt")
  writeLines("hello world", local_file)

  remote_url <- sftp_test_url("test_file.txt")
  sftp_put(local_file, remote_url)

  downloaded_file <- withr::local_tempfile(fileext = ".txt")
  sftp_get(remote_url, downloaded_file)

  expect_equal(readLines(downloaded_file), "hello world")
})

test_that("sftp_exists returns TRUE for existing files", {
  local_file <- withr::local_tempfile(fileext = ".txt")
  writeLines("test content", local_file)

  remote_url <- sftp_test_url("exists_test.txt")
  sftp_put(local_file, remote_url)

  expect_true(sftp_exists(remote_url))
})

test_that("sftp_exists returns FALSE for non-existent files", {
  remote_url <- sftp_test_url("nonexistent_file.txt")

  expect_false(sftp_exists(remote_url))
})
