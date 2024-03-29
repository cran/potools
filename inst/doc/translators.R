## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(potools)

## -----------------------------------------------------------------------------
name <- "Michael"
glue::glue("Hi {name}!")

## -----------------------------------------------------------------------------
name <- "Michael"
sprintf("Hi %s!", name)

## -----------------------------------------------------------------------------
sprintf("%s %s %s", "first", "second", "third")
sprintf("%2$s %1$s %3$s", "first", "second", "third")

