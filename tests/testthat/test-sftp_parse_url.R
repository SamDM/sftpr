test_that("sftp_parse_url parses valid URLs", {
  url <- "sftp://user@host/path/to/file"
  parsed <- sftp_parse_url(url)
  expect_equal(parsed$user, "user")
  expect_equal(parsed$host, "host")
  expect_null(parsed$port)
  expect_equal(parsed$remote_path, "/path/to/file")
})

test_that("sftp_parse_url parses URLs with port", {
  url <- "sftp://user@host:2222/path/to/file"
  parsed <- sftp_parse_url(url)
  expect_equal(parsed$user, "user")
  expect_equal(parsed$host, "host")
  expect_equal(parsed$port, 2222L)
  expect_equal(parsed$remote_path, "/path/to/file")
})

test_that("sftp_parse_url handles invalid URLs", {
  invalid_url <- "sftp://invalid-url"
  expect_error(sftp_parse_url(invalid_url), "Invalid SFTP URL format")

  incomplete_url <- "sftp://user@host"
  expect_error(sftp_parse_url(incomplete_url), "Invalid SFTP URL format")
})
