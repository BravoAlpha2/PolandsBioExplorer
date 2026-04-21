
# Author: Nuno Garcia
# Date: 21/04/2026
# LinkedIn: https://www.linkedin.com/in/nuno-garcia-97b780158/
# ORCID: https://orcid.org/0000-0001-7917-3286
#
# Script purpose:
# Testing the app´s logic for the:

# Shiny app (main):
# Dashboard to explore biodiversity observations in Poland.
# Users can search species by vernacularName and scientificName,
# inspect observations on a map, and view a temporal timeline.

# Packages
library(testthat)
library(dplyr)
library(lubridate)

source(testthat::test_path("../../R scripts/helpers.R"))

test_that("filter_observation_data filters records with media", {
  dat <- data.frame(
    scientific_name = c("A", "A"),
    observation_year = c(2001, 2002),
    media_url = c("img.jpg", ""),
    stringsAsFactors = FALSE
  )
  
  res <- filter_observation_data(
    dat = dat,
    year_range = NULL,
    only_with_media = TRUE
  )
  
  expect_equal(nrow(res), 1)
  expect_equal(res$media_url[[1]], "img.jpg")
})

test_that("filter_observation_data filters by year range", {
  dat <- data.frame(
    scientific_name = c("A", "A", "A"),
    observation_year = c(1999, 2005, 2010),
    media_url = c("", "", ""),
    stringsAsFactors = FALSE
  )
  
  res <- filter_observation_data(
    dat = dat,
    year_range = c(2000, 2008),
    only_with_media = FALSE
  )
  
  expect_equal(nrow(res), 1)
  expect_equal(res$observation_year[[1]], 2005)
})

test_that("filter_observation_data handles empty input", {
  dat <- data.frame(
    scientific_name = character(0),
    observation_year = numeric(0),
    media_url = character(0)
  )
  
  res <- filter_observation_data(
    dat = dat,
    year_range = c(2000, 2020),
    only_with_media = TRUE
  )
  
  expect_equal(nrow(res), 0)
})

test_that("build_timeline_data returns yearly counts", {
  dat <- data.frame(
    observation_year = c(2001, 2001, 2002, NA)
  )
  
  res <- build_timeline_data(dat)
  
  expect_equal(nrow(res), 2)
  expect_equal(res$observation_year, c(2001, 2002))
  expect_equal(res$n_obs, c(2, 1))
})

test_that("build_timeline_data handles empty input", {
  dat <- data.frame(observation_year = numeric(0))
  
  res <- build_timeline_data(dat)
  
  expect_equal(nrow(res), 0)
  expect_true(all(c("observation_year", "n_obs") %in% names(res)))
})

test_that("build_selection_summary returns default summary", {
  res <- build_selection_summary(
    dat = NULL,
    selected_species = NULL,
    default_obs_count = 999
  )
  
  expect_equal(res$title, "National overview")
  expect_equal(res$subtitle, "No species selected")
  expect_equal(res$obs_n, 999)
  expect_null(res$years)
})

test_that("build_selection_summary returns selected species summary", {
  dat <- data.frame(
    scientific_name = c("Lynx lynx", "Lynx lynx"),
    observation_year = c(2005, 2010)
  )
  
  res <- build_selection_summary(
    dat = dat,
    selected_species = "Lynx lynx",
    default_obs_count = 999
  )
  
  expect_equal(res$title, "Lynx lynx")
  expect_equal(res$subtitle, "Selected species")
  expect_equal(res$obs_n, 2)
  expect_equal(res$years, "2005 - 2010")
})