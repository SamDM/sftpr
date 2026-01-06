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

test_that("sftp_delete removes remote files", {
  local_file <- withr::local_tempfile(fileext = ".txt")
  writeLines("to be deleted", local_file)

  remote_url <- sftp_test_url("delete_test.txt")
  sftp_put(local_file, remote_url)
  expect_true(sftp_exists(remote_url))

  sftp_delete(remote_url)
  expect_false(sftp_exists(remote_url))
})

test_that("sftp_mkdir, sftp_exists, and sftp_rmdir work for directories", {
  dir_url <- sftp_test_url("test_subdir")

  sftp_mkdir(dir_url)
  expect_true(sftp_exists(dir_url))

  sftp_rmdir(dir_url)
  expect_false(sftp_exists(dir_url))
})

test_that("sftp_rename moves files", {
  local_file <- withr::local_tempfile(fileext = ".txt")
  writeLines("rename me", local_file)

  old_url <- sftp_test_url("rename_old.txt")
  new_url <- sftp_test_url("rename_new.txt")

  sftp_put(local_file, old_url)
  sftp_rename(old_url, new_url)

  expect_false(sftp_exists(old_url))
  expect_true(sftp_exists(new_url))

  # Verify content preserved
  downloaded <- withr::local_tempfile(fileext = ".txt")
  sftp_get(new_url, downloaded)
  expect_equal(readLines(downloaded), "rename me")
})

test_that("sftp_ls lists directory contents", {
  # Create a subdirectory with files
  subdir_url <- sftp_test_url("ls_test_dir")
  sftp_mkdir(subdir_url)

  local_file <- withr::local_tempfile(fileext = ".txt")
  writeLines("content", local_file)

  sftp_put(local_file, paste0(subdir_url, "/file1.txt"))
  sftp_put(local_file, paste0(subdir_url, "/file2.txt"))

  files <- sftp_ls(subdir_url)
  expect_true("file1.txt" %in% files)
  expect_true("file2.txt" %in% files)
})

test_that("sftp_stat returns file metadata", {
  local_file <- withr::local_tempfile(fileext = ".txt")
  writeLines("stat test content", local_file)

  remote_url <- sftp_test_url("stat_test.txt")
  sftp_put(local_file, remote_url)

  info <- sftp_stat(remote_url)

  expect_type(info, "list")
  expect_true(info$size > 0)
  expect_true(nzchar(info$permissions))
  expect_true(nzchar(info$mtime))
  expect_equal(info$name, "stat_test.txt")
})

test_that("sftp_chmod changes file permissions", {
  local_file <- withr::local_tempfile(fileext = ".txt")
  writeLines("chmod test", local_file)

  remote_url <- sftp_test_url("chmod_test.txt")
  sftp_put(local_file, remote_url)

  sftp_chmod(remote_url, "644")
  info1 <- sftp_stat(remote_url)
  expect_match(info1$permissions, "^-rw-r--r--")

  sftp_chmod(remote_url, "755")
  info2 <- sftp_stat(remote_url)
  expect_match(info2$permissions, "^-rwxr-xr-x")
})

