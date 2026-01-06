#' Parse an SFTP URL into components
#'
#' @param sftp_url SFTP URL in format: sftp://user@host/path/to/file
#' @return List with user, host, remote_path
sftp_parse_url <- function(sftp_url) {
  url_pattern <- "^sftp://([^@]+)@([^/:]+)(:([0-9]+))?(/.*)$"
  if (!grepl(url_pattern, sftp_url)) {
    stop("Invalid SFTP URL format. Expected: sftp://user@host[:port]/path/to/file")
  }

  port_str <- sub(url_pattern, "\\4", sftp_url)
  list(
    user = sub(url_pattern, "\\1", sftp_url),
    host = sub(url_pattern, "\\2", sftp_url),
    port = if (nzchar(port_str)) as.integer(port_str) else NULL,
    remote_path = sub(url_pattern, "\\5", sftp_url)
  )
}

#' Execute an SFTP batch command
#'
#' @param commands Character vector of SFTP commands
#' @param user SFTP username
#' @param host SFTP host
#' @param port Optional SFTP port number (default: NULL uses standard port 22)
#' @param ssh_key_path Optional path to SSH private key file for authentication
#' @param error_msg Error message prefix on failure
#' @return Invisible NULL on success, stops with error on failure
sftp_batch <- function(commands, user, host, port = NULL, ssh_key_path = NULL,
                       error_msg = "SFTP command failed") {
  batch_file <- tempfile(fileext = ".sftp")
  on.exit(unlink(batch_file), add = TRUE)

  writeLines(c(commands, "bye"), batch_file)

  sftp_args <- c("-b", batch_file)
  if (!is.null(port)) {
    sftp_args <- c(sftp_args, "-P", as.character(port))
  }
  if (!is.null(ssh_key_path)) {
    sftp_args <- c(sftp_args, "-i", ssh_key_path)
  }
  sftp_args <- c(sftp_args, sprintf("%s@%s", user, host))

  result <- system2(
    "sftp",
    args = sftp_args,
    stdout = TRUE,
    stderr = TRUE
  )

  exit_status <- attr(result, "status")
  if (!is.null(exit_status) && exit_status != 0) {
    stop(sprintf(
      "%s (exit code %d):\n%s",
      error_msg,
      exit_status,
      paste(result, collapse = "\n")
    ))
  }

  invisible(NULL)
}

#' Upload a local file to an SFTP server
#'
#' @param local_path Path to the local file to upload
#' @param sftp_url SFTP URL in format: sftp://user@host/path/to/file
#' @param ssh_key_path Optional path to SSH private key file for authentication
#' @return Invisible NULL on success, stops with error on failure
#'
#' @export
sftp_put <- function(local_path, sftp_url, ssh_key_path = NULL) {
  stopifnot(file.exists(local_path))

  parsed <- sftp_parse_url(sftp_url)

  sftp_batch(
    sprintf("put %s %s", local_path, parsed$remote_path),
    parsed$user,
    parsed$host,
    parsed$port,
    ssh_key_path = ssh_key_path,
    "SFTP upload failed"
  )
}

#' Download a file from an SFTP server
#'
#' @param sftp_url SFTP URL in format: sftp://user@host/path/to/file
#' @param local_path Path to save the downloaded file
#' @param ssh_key_path Optional path to SSH private key file for authentication
#' @return Invisible NULL on success, stops with error on failure
#'
#' @export
sftp_get <- function(sftp_url, local_path, ssh_key_path = NULL) {
  parsed <- sftp_parse_url(sftp_url)

  sftp_batch(
    sprintf("get %s %s", parsed$remote_path, local_path),
    parsed$user,
    parsed$host,
    parsed$port,
    ssh_key_path = ssh_key_path,
    "SFTP download failed"
  )
}

# Resolve and validate path_arg for sftp_writer/sftp_reader
# path_arg_expr should be substitute(path_arg) from the caller
.resolve_path_arg <- function(fn, path_arg_expr, path_arg_missing) {

  arg_names <- names(formals(fn))

  # Handle unquoted path_arg using non-standard evaluation
  path_arg <- if (!path_arg_missing) as.character(path_arg_expr) else NULL

  # Auto-detect path_arg if not provided
  if (is.null(path_arg)) {
    path_arg <- arg_names[grepl("file|path", arg_names, ignore.case = TRUE)][1]
    if (is.na(path_arg)) {
      stop("Could not auto-detect path argument. Please specify `path_arg` explicitly.")
    }
  }

  # Validate path_arg exists in fn's formals
  if (!path_arg %in% arg_names) {
    stop(sprintf(
      "Argument '%s' not found in function formals: %s",
      path_arg, paste(arg_names, collapse = ", ")
    ))
  }

  path_arg
}

#' Wrap an R function that saves an object to a file, such that it works over SFTP
#'
#' Creates a wrapper around a file-writing function that transparently handles
#' SFTP URLs. Local paths are passed through unchanged to the original function.
#'
#' @param fn A function that writes to a file (e.g., readr::write_tsv, saveRDS)
#' @param path_arg The name of the file path argument in fn. Can be unquoted or
#'   a string. If NULL (default), auto-detects the first argument containing
#'   "file" or "path" in its name.
#' @return A wrapped function with the same signature as fn
#'
#' @examples
#' \dontrun{
#' write_tsv_sftp <- sftp_writer(readr::write_tsv)
#' write_tsv_sftp(my_df, "sftp://user@host/path/to/data.tsv.gz")
#' write_tsv_sftp(my_df, "/local/path/data.tsv")  # Also works locally
#' }
#'
#' # Explicit path_arg (unquoted or quoted)
#' saveRDS_sftp <- sftp_writer(saveRDS, file)
#' saveRDS_sftp <- sftp_writer(saveRDS, "file")
#'
#' @export
sftp_writer <- function(fn, path_arg = NULL) {
  path_arg <- .resolve_path_arg(fn, substitute(path_arg), missing(path_arg))

  function(...) {
    call_args <- as.list(match.call(fn, call = match.call()))[-1]
    call_args <- lapply(call_args, eval, envir = parent.frame())

    file_path <- call_args[[path_arg]]

    # Local paths: pass through to original function
    if (!grepl("^sftp://", file_path)) {
      return(fn(...))
    }

    # SFTP: write to temp file, then upload
    remote_path <- sftp_parse_url(file_path)$remote_path
    temp_file <- tempfile(fileext = basename(remote_path))
    on.exit(unlink(temp_file), add = TRUE)

    call_args[[path_arg]] <- temp_file
    result <- rlang::exec(fn, !!!call_args)

    sftp_put(temp_file, file_path)
    result
  }
}

#' Check if a file or directory exists on an SFTP server
#'
#' @param sftp_url SFTP URL in format: sftp://user@host/path/to/file
#' @param ssh_key_path Optional path to SSH private key file for authentication
#' @return TRUE if the file/directory exists, FALSE otherwise
#'
#' @examples
#' \dontrun{
#' sftp_exists("sftp://user@host/path/to/file.txt")
#' sftp_exists("sftp://user@host/path/to/directory")
#' }
#'
#' @export
sftp_exists <- function(sftp_url, ssh_key_path = NULL) {
  parsed <- sftp_parse_url(sftp_url)

  withCallingHandlers(
    tryCatch(
      {
        sftp_batch(
          sprintf("ls %s", parsed$remote_path),
          parsed$user,
          parsed$host,
          parsed$port,
          ssh_key_path = ssh_key_path,
          error_msg = "SFTP exists check failed"
        )
        TRUE
      },
      error = function(e) {
        if (grepl("not found|No such file", e$message, ignore.case = TRUE)) {
          FALSE
        } else {
          stop(e)
        }
      }
    ),
    warning = function(w) {
      # Suppress "had status 1" warnings from system2 (expected for non-existent files)
      if (grepl("had status 1", w$message, ignore.case = TRUE)) {
        invokeRestart("muffleWarning")
      }
    }
  )
}

#' Delete a file on an SFTP server
#'
#' @param sftp_url SFTP URL in format: sftp://user@host/path/to/file
#' @param ssh_key_path Optional path to SSH private key file for authentication
#' @return Invisible NULL on success, stops with error on failure
#'
#' @examples
#' \dontrun{
#' sftp_delete("sftp://user@host/path/to/file.txt")
#' }
#'
#' @export
sftp_delete <- function(sftp_url, ssh_key_path = NULL) {
  parsed <- sftp_parse_url(sftp_url)

  sftp_batch(
    sprintf("rm %s", parsed$remote_path),
    parsed$user,
    parsed$host,
    parsed$port,
    ssh_key_path = ssh_key_path,
    error_msg = "SFTP delete failed"
  )
}

#' Create a directory on an SFTP server
#'
#' @param sftp_url SFTP URL in format: sftp://user@host/path/to/directory
#' @param ssh_key_path Optional path to SSH private key file for authentication
#' @return Invisible NULL on success, stops with error on failure
#'
#' @examples
#' \dontrun{
#' sftp_mkdir("sftp://user@host/path/to/newdir")
#' }
#'
#' @export
sftp_mkdir <- function(sftp_url, ssh_key_path = NULL) {
  parsed <- sftp_parse_url(sftp_url)

  sftp_batch(
    sprintf("mkdir %s", parsed$remote_path),
    parsed$user,
    parsed$host,
    parsed$port,
    ssh_key_path = ssh_key_path,
    error_msg = "SFTP mkdir failed"
  )
}

#' Remove a directory on an SFTP server
#'
#' @param sftp_url SFTP URL in format: sftp://user@host/path/to/directory
#' @param ssh_key_path Optional path to SSH private key file for authentication
#' @return Invisible NULL on success, stops with error on failure
#'
#' @examples
#' \dontrun{
#' sftp_rmdir("sftp://user@host/path/to/emptydir")
#' }
#'
#' @export
sftp_rmdir <- function(sftp_url, ssh_key_path = NULL) {
  parsed <- sftp_parse_url(sftp_url)

  sftp_batch(
    sprintf("rmdir %s", parsed$remote_path),
    parsed$user,
    parsed$host,
    parsed$port,
    ssh_key_path = ssh_key_path,
    error_msg = "SFTP rmdir failed"
  )
}

#' Rename or move a file on an SFTP server
#'
#' @param from_url SFTP URL of the source file
#' @param to_url SFTP URL of the destination (must be same host/user as from_url)
#' @param ssh_key_path Optional path to SSH private key file for authentication
#' @return Invisible NULL on success, stops with error on failure
#'
#' @examples
#' \dontrun{
#' sftp_rename("sftp://user@host/old/path.txt", "sftp://user@host/new/path.txt")
#' }
#'
#' @export
sftp_rename <- function(from_url, to_url, ssh_key_path = NULL) {
  from_parsed <- sftp_parse_url(from_url)
  to_parsed <- sftp_parse_url(to_url)

  if (from_parsed$user != to_parsed$user || from_parsed$host != to_parsed$host) {
    stop("from_url and to_url must have the same user and host")
  }

  sftp_batch(
    sprintf("rename %s %s", from_parsed$remote_path, to_parsed$remote_path),
    from_parsed$user,
    from_parsed$host,
    from_parsed$port,
    ssh_key_path = ssh_key_path,
    error_msg = "SFTP rename failed"
  )
}

#' Change file permissions on an SFTP server
#'
#' @param sftp_url SFTP URL in format: sftp://user@host/path/to/file
#' @param mode Permission mode as octal string (e.g., "755") or integer (e.g., 755)
#' @param ssh_key_path Optional path to SSH private key file for authentication
#' @return Invisible NULL on success, stops with error on failure
#'
#' @examples
#' \dontrun{
#' sftp_chmod("sftp://user@host/path/to/file.txt", "755")
#' sftp_chmod("sftp://user@host/path/to/file.txt", 644)
#' }
#'
#' @export
sftp_chmod <- function(sftp_url, mode, ssh_key_path = NULL) {
  parsed <- sftp_parse_url(sftp_url)
  mode_str <- as.character(mode)

  if (!grepl("^[0-7]{3,4}$", mode_str)) {
    stop("mode must be a valid octal permission (e.g., '755' or '0644')")
  }

  sftp_batch(
    sprintf("chmod %s %s", mode_str, parsed$remote_path),
    parsed$user,
    parsed$host,
    parsed$port,
    ssh_key_path = ssh_key_path,
    error_msg = "SFTP chmod failed"
  )
}

#' List files in a directory on an SFTP server
#'
#' @param sftp_url SFTP URL in format: sftp://user@host/path/to/directory
#' @param ssh_key_path Optional path to SSH private key file for authentication
#' @return Character vector of filenames in the directory
#'
#' @examples
#' \dontrun{
#' sftp_ls("sftp://user@host/path/to/directory")
#' }
#'
#' @export
sftp_ls <- function(sftp_url, ssh_key_path = NULL) {
  parsed <- sftp_parse_url(sftp_url)

  batch_file <- tempfile(fileext = ".sftp")
  on.exit(unlink(batch_file), add = TRUE)

  writeLines(c(sprintf("ls %s", parsed$remote_path), "bye"), batch_file)

  sftp_args <- c("-b", batch_file)
  if (!is.null(parsed$port)) {
    sftp_args <- c(sftp_args, "-P", as.character(parsed$port))
  }
  if (!is.null(ssh_key_path)) {
    sftp_args <- c(sftp_args, "-i", ssh_key_path)
  }
  sftp_args <- c(sftp_args, sprintf("%s@%s", parsed$user, parsed$host))

  result <- system2("sftp", args = sftp_args, stdout = TRUE, stderr = TRUE)

  exit_status <- attr(result, "status")
  if (!is.null(exit_status) && exit_status != 0) {
    stop(sprintf(
      "SFTP ls failed (exit code %d):\n%s",
      exit_status,
      paste(result, collapse = "\n")
    ))
  }

  # Parse output: skip header lines, extract filenames
  # Output returns full paths, multiple per line separated by whitespace
  lines <- result[!grepl("^sftp>", result)]
  lines <- trimws(lines)
  lines <- lines[nzchar(lines)]
  # Split each line by whitespace and flatten
  paths <- unlist(strsplit(lines, "\\s+"))
  paths <- paths[nzchar(paths)]
  basename(paths)
}

#' Get file metadata from an SFTP server
#'
#' @param sftp_url SFTP URL in format: sftp://user@host/path/to/file
#' @param ssh_key_path Optional path to SSH private key file for authentication
#' @return List with: name, size, mtime, permissions; or NULL if file not found
#'
#' @examples
#' \dontrun{
#' info <- sftp_stat("sftp://user@host/path/to/file.txt")
#' info$size
#' info$mtime
#' }
#'
#' @export
sftp_stat <- function(sftp_url, ssh_key_path = NULL) {
  parsed <- sftp_parse_url(sftp_url)

  batch_file <- tempfile(fileext = ".sftp")
  on.exit(unlink(batch_file), add = TRUE)

  writeLines(c(sprintf("ls -l %s", parsed$remote_path), "bye"), batch_file)

  sftp_args <- c("-b", batch_file)
  if (!is.null(parsed$port)) {
    sftp_args <- c(sftp_args, "-P", as.character(parsed$port))
  }
  if (!is.null(ssh_key_path)) {
    sftp_args <- c(sftp_args, "-i", ssh_key_path)
  }
  sftp_args <- c(sftp_args, sprintf("%s@%s", parsed$user, parsed$host))

  result <- system2("sftp", args = sftp_args, stdout = TRUE, stderr = TRUE)

  exit_status <- attr(result, "status")
  if (!is.null(exit_status) && exit_status != 0) {
    if (grepl("not found|No such file", paste(result, collapse = "\n"), ignore.case = TRUE)) {
      return(NULL)
    }
    stop(sprintf(
      "SFTP stat failed (exit code %d):\n%s",
      exit_status,
      paste(result, collapse = "\n")
    ))
  }

  # Parse ls -l output: -rw-r--r--    1 user group  size Mon DD HH:MM filename
  lines <- result[!grepl("^sftp>", result)]
  lines <- trimws(lines)
  lines <- lines[nzchar(lines)]

  if (length(lines) == 0) return(NULL)

  # Find the line with file info (starts with permission string like -rw or drw)
  info_line <- lines[grepl("^[-dlrwxs]", lines)][1]
  if (is.na(info_line)) return(NULL)

  parts <- strsplit(info_line, "\\s+")[[1]]
  # Typical format: perms links user group size month day time name
  if (length(parts) < 9) return(NULL)

  list(
    permissions = parts[1],
    size = as.numeric(parts[5]),
    mtime = paste(parts[6:8], collapse = " "),
    name = basename(paste(parts[9:length(parts)], collapse = " "))
  )
}

#' Wrap an R function that reads from a file, such that it works over SFTP
#'
#' Creates a wrapper around a file-reading function that transparently handles
#' SFTP URLs. Local paths are passed through unchanged to the original function.
#'
#' @param fn A function that reads from a file (e.g., readr::read_tsv, readRDS)
#' @param path_arg The name of the file path argument in fn. Can be unquoted or
#'   a string. If NULL (default), auto-detects the first argument containing
#'   "file" or "path" in its name.
#' @return A wrapped function with the same signature as fn
#'
#' @examples
#' \dontrun{
#' read_tsv_sftp <- sftp_reader(readr::read_tsv)
#' read_tsv_sftp("sftp://user@host/path/to/data.tsv.gz")
#' read_tsv_sftp("/local/path/data.tsv")  # Also works locally
#' }
#'
#' # Explicit path_arg (unquoted or quoted)
#' readRDS_sftp <- sftp_reader(readRDS, file)
#' readRDS_sftp <- sftp_reader(readRDS, "file")
#'
#' @export
sftp_reader <- function(fn, path_arg = NULL) {
  path_arg <- .resolve_path_arg(fn, substitute(path_arg), missing(path_arg))

  function(...) {
    # Capture and evaluate all arguments
    call_args <- as.list(match.call(fn, call = match.call()))[-1]
    call_args <- lapply(call_args, eval, envir = parent.frame())

    file_path <- call_args[[path_arg]]

    # Local paths: pass through to original function
    if (!grepl("^sftp://", file_path)) {
      return(fn(...))
    }

    # SFTP: download to temp file, then read
    remote_path <- sftp_parse_url(file_path)$remote_path
    temp_file <- tempfile(fileext = basename(remote_path))
    on.exit(unlink(temp_file), add = TRUE)

    sftp_get(file_path, temp_file)

    call_args[[path_arg]] <- temp_file
    rlang::exec(fn, !!!call_args)
  }
}
