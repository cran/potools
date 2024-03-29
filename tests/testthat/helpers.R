# copy a package to tmp, then overwrite any changes on exit
restore_package <- function(dir, expr, tmp_conn) {
  # this is way uglier than it should be. i'm missing something.
  tdir <- tempdir()

  file.copy(dir, tdir, recursive = TRUE)
  on.exit({
    unlink(dir, recursive = TRUE)
    dir.create(dir)
    file.copy(file.path(tdir, basename(dir)), dirname(dir), recursive = TRUE)
    unlink(file.path(tdir, basename(dir)), recursive = TRUE)
  })

  if (!missing(tmp_conn)) {
    old = options("__potools_testing_prompt_connection__" = tmp_conn)
    on.exit(options(old), add = TRUE)
  }

  invisible(capture.output(expr))
}

# TODO: I think this can just be replaced by expect_match and expect_no_match in current testthat dev
expect_all_match = function(inputs, targets, ..., invert=FALSE) {
  matched <- vapply(
    targets,
    function(target) length(grep(target, inputs, ..., invert=invert)) > 0L,
    logical(1L)
  )

  expect(
    all(matched),
    sprintf(
      if (invert) {
        "Unwanted messages found:\n  Observed: %s\n  Didn't want: %s\n"
      } else {
        "Not all messages found:\n  Observed: %s\n  Wanted: %s\n"
      },
      toString(sQuote(inputs)),
      toString(sQuote(targets[!matched]))
    )
  )
}

expect_messages = function(expr, msgs, ..., invert=FALSE) {
  observed_messages = capture_messages(expr)
  expect_all_match(observed_messages, msgs, ..., invert=invert)
}

test_package = function(pkg) test_path(file.path("test_packages", pkg))
mock_translation = function(mocks) test_path(file.path("mock_translations", mocks))

local_test_package <- function(..., .envir = parent.frame()) {
  temp <- withr::local_tempdir(.local_envir = .envir)
  writeLines(con = file.path(temp, "DESCRIPTION"), c(
    "Package: test",
    "Version: 1.0.0"
  ))
  dir_create(file.path(temp, c("po", "R")))

  files <- list(...)
  for (i in seq_along(files)) {
    writeLines(files[[i]], file.path(temp, names(files)[[i]]))
  }

  temp
}

# different platforms/installations of gettext apparently
#   produce a different number of "." in "progress" output; normalize
standardize_dots <- standardise_dots <- function(x) {
  gsub("\\.{2,}", ".", x)
}
