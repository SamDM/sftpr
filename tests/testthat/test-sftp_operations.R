library(testthat)
source("../../R/sftp.R")
source("helper-sftp.R")

# Setup/Teardown: Start/Stop SFTP server for all tests in this file
setup(start_sftp_server())
teardown(stop_sftp_server())

# --- Test Definitions ---

describe("sftp_put and sftp_get (Integration Tests)", {
  it("can upload and then download a file", {
    # 1. Setup
    local_upload_path <- tempfile(fileext = ".txt")
    local_download_path <- tempfile(fileext = ".txt")
    on.exit({
      unlink(local_upload_path)
      unlink(local_download_path)
    }, add = TRUE)
    
    test_string <- "Hello SFTP world!"
    writeLines(test_string, local_upload_path)
    
    sftp_target_url <- sprintf("sftp://%s @127.0.0.1:%d/uploads/test_file.txt", .sftp_user_name, .sftp_port)
    
    # 2. Upload the file
    expect_no_error(
      sftp_put(local_upload_path, sftp_target_url, ssh_key_path = .sftp_client_private_key)
    )
    
    # 3. Verify upload on server-side
    server_file_path <- file.path(.sftp_uploads_dir, "test_file.txt")
    expect_true(file.exists(server_file_path))
    expect_equal(readLines(server_file_path), test_string)
    
    # 4. Download the file
    expect_no_error(
      sftp_get(sftp_target_url, local_download_path, ssh_key_path = .sftp_client_private_key)
    )
    
    # 5. Verify download
    expect_true(file.exists(local_download_path))
    expect_equal(readLines(local_download_path), test_string)
  })
  
  it("sftp_get fails for a non-existent file", {
    local_download_path <- tempfile(fileext = ".txt")
    on.exit(unlink(local_download_path), add = TRUE)
    
    non_existent_url <- sprintf("sftp://%s @127.0.0.1:%d/uploads/non_existent.txt", .sftp_user_name, .sftp_port)
    
    expect_error(
      sftp_get(non_existent_url, local_download_path, ssh_key_path = .sftp_client_private_key),
      regexp = "SFTP download failed"
    )
  })
  
  it("sftp_put fails for a non-existent local file", {
    sftp_target_url <- sprintf("sftp://%s @127.0.0.1:%d/uploads/test_file.txt", .sftp_user_name, .sftp_port)
    expect_error(
      sftp_put("nonexistent_local.txt", sftp_target_url, ssh_key_path = .sftp_client_private_key)
    )
  })
})


