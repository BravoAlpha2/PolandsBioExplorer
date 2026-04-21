
# Author: Nuno Garcia
# Date: 21/04/2026
# LinkedIn: https://www.linkedin.com/in/nuno-garcia-97b780158/
# ORCID: https://orcid.org/0000-0001-7917-3286
#
# Script purpose:
# Testing the helpers for the:

# Shiny app (main):
# Dashboard to explore biodiversity observations in Poland.
# Users can search species by vernacularName and scientificName,
# inspect observations on a map, and view a temporal timeline.

# Packages
library(testthat)
library(dplyr)
library(lubridate)

source(testthat::test_path("../../R scripts/helpers.R"))

test_that("mode_value returns most frequent value", {
  x <- c("A", "A", "B")
  expect_equal(mode_value(x), "A")
})

test_that("mode_value ignores NA values", {
  x <- c(NA, "A", "A", "B")
  expect_equal(mode_value(x), "A")
})

test_that("fallback_value returns fallback for NA or empty", {
  expect_equal(fallback_value(NA, "x"), "x")
  expect_equal(fallback_value("", "x"), "x")
  expect_equal(fallback_value("ok", "x"), "ok")
})