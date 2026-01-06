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

test_that("sftp_writer and sftp_reader round-trip works with writeLines/readLines", {
  content <- c("line one", "line two", "line three")
  remote_url <- sftp_test_url("wrapper_roundtrip.txt")

  # Write to SFTP using wrapped writeLines
  write_lines_sftp <- sftp_writer(writeLines, con)
  write_lines_sftp(content, remote_url)

  # Read back using wrapped readLines
  read_lines_sftp <- sftp_reader(readLines, con)
  result <- read_lines_sftp(remote_url)

  expect_equal(result, content)
})

test_that("sftp_writer and sftp_reader round-trip works with saveRDS/readRDS", {
  data <- list(a = 1:3, b = "test", c = data.frame(x = 1:2, y = c("a", "b")))
  remote_url <- sftp_test_url("wrapper_roundtrip.rds")

  # Write to SFTP using wrapped saveRDS
  save_rds_sftp <- sftp_writer(saveRDS)
  save_rds_sftp(data, remote_url)

  # Read back using wrapped readRDS
  read_rds_sftp <- sftp_reader(readRDS)
  result <- read_rds_sftp(remote_url)

  expect_equal(result, data)
})

test_that("sftp_reader passes additional arguments to wrapped function", {
  # Upload a file with more lines
  local_file <- withr::local_tempfile(fileext = ".txt")
  writeLines(c("a", "b", "c", "d", "e"), local_file)

  remote_url <- sftp_test_url("reader_args_test.txt")
  sftp_put(local_file, remote_url)

  # Use readLines with n argument to read only first 2 lines
  read_lines_sftp <- sftp_reader(readLines, con)
  result <- read_lines_sftp(remote_url, n = 2)

  expect_equal(result, c("a", "b"))
})

