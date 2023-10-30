## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(glue)

## ----setup--------------------------------------------------------------------
library(potools)

## ----error = TRUE-------------------------------------------------------------
message("This", " is", " a", " message")
warning("This", " is", " a", " warning")
stop("This", " is", " an", " error")

## -----------------------------------------------------------------------------
messagef <- function(fmt, ..., appendLF = TRUE) {
  msg <- gettextf(fmt, ..., domain = "R-{mypackage}")
  message(msg, domain = NA, appendLF = appendLF)
}

warningf <- function(fmt, ..., immediate. = FALSE, noBreaks. = FALSE) {
  msg <- gettextf(fmt, ..., domain = "R-{mypackage}")
  warning(msg,
    domain = NA,
    call. = FALSE,
    immediate. = immediate.,
    noBreaks. = noBreaks.
  )
}

stopf <- function(fmt, ...) {
  msg <- gettextf(fmt, ..., domain = "R-{mypackage}")
  stop(msg, domain = NA, call. = FALSE)
}

## -----------------------------------------------------------------------------
tr_ <- function(...) {
  enc2utf8(gettext(paste0(...), domain = "R-{mypackage}"))
}

## -----------------------------------------------------------------------------
name <- "Hadley"
paste0(tr_("Good"), " ", tr_("morning"), " ", name, "!")

## -----------------------------------------------------------------------------
glue(tr_("Good morning {name}"))
sprintf(tr_("Good morning %s"), name)

## -----------------------------------------------------------------------------
greet <- function(name, time_of_day) {
  paste0(tr_("Good"), " ", time_of_day, " ", name, "!")
}
greet("Hadley", tr_("morning"))
greet("Hadley", tr_("afternoon"))
greet("Hadley", tr_("evening"))

## -----------------------------------------------------------------------------
greet <- function(name, time_of_day) {
  switch(time_of_day,
    morning = glue(tr_("Good morning {name}!")),
    afternoon = glue(tr_("Good afternoon {name}!")),
    evening = glue(tr_("Good evening {name}!"))
  )
}

## ----results = FALSE----------------------------------------------------------
# Instead of this:
tr_("See <https://r-project.org> to learn more")

# Try this:
url <- "https://r-project.org"
glue(tr_("See <{url}> to learn more"))

## ----results = FALSE----------------------------------------------------------
# Instead of this:
tr_("<a href='/index.html'>Home page</a>")

# Try this:
paste0("<a href='/index.html'>", tr_("Home page"), "</a>")

## -----------------------------------------------------------------------------
cows <- function(n) {
  if (n == 1) {
    paste0(n, " cow")
  } else {
    paste0(n, " cows")
  }
}
paste("I have ", cows(0))
paste("I have ", cows(1))
paste("I have ", cows(2))

## -----------------------------------------------------------------------------
paste0("There are ", cows(0), " in the field")
paste0("There are ", cows(1), " in the field")

## -----------------------------------------------------------------------------
field_cows <- function(n) {
  if (n == 1) {
    fmt <- tr_("There is {n} cow in the field")
  } else {
    fmt <- tr_("There are {n} cows in the field")
  }
  glue(fmt)
}

## -----------------------------------------------------------------------------
field_cows <- function(n) {
  glue(ngettext(n,
    "There is {n} cow in the field",
    "There are {n} cows in the field"
  ))
}

## ----eval = FALSE-------------------------------------------------------------
#  library(and)
#  values <- c("first", "middle", "last")
#  or(values)
#  #> [1] "first, middle, and last"
#  
#  # lang is normally retrieve automatically from the environemtn
#  # overriding it here to show what a translation looks like:
#  or(values, lang = "fr")
#  #> [1] "first, middle ou last"

## ----eval = FALSE-------------------------------------------------------------
#  glue(tr_("`x` must be one of {and(values)}"))
#  #> `x` must be one of first, middle and last

