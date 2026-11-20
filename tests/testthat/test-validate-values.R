# Offline checks of the value-level validation in TableSchema$validate(),
# using an in-memory schema so that no internet access is needed.

line_break_schema <- function() {
  TableSchema$new("observations", json = list(
    name = "observations",
    fields = list(
      list(name = "observationID", type = "string", constraints = list(required = TRUE)),
      list(name = "count", type = "integer"),
      list(name = "observationComments", type = "string")),
    primaryKey = "observationID"))
}

test_that("line breaks in text values give warnings, not errors", {
  df <- data.frame(observationID = c("o1", "o2", "o3", "o4"),
                   count = c(1L, 2L, 3L, NA),
                   observationComments = c("one line", "two\nlines", "windows\r\nbreak", NA))
  issues <- line_break_schema()$validate(df, source = "observations.csv")
  lb <- issues[issues$constraint == "lineBreak", ]
  expect_equal(lb$row, c(2L, 3L))
  expect_true(all(lb$field == "observationComments"))
  expect_true(all(lb$severity == "warning"))
  expect_match(lb$message, "line break", all = TRUE)
  expect_true(ctdp_is_valid(issues))
})

test_that("line breaks in custom (non-schema) columns also give warnings", {
  df <- data.frame(observationID = c("o1", "o2"), count = c(1L, 2L),
                   comment = c("first\nline", NA))
  issues <- line_break_schema()$validate(df, source = "observations.csv")
  lb <- issues[issues$constraint == "lineBreak", ]
  expect_equal(lb$field, "comment")
  expect_equal(lb$row, 1L)
  expect_equal(lb$severity, "warning")
})

test_that("values without line breaks give no lineBreak issue", {
  df <- data.frame(observationID = c("o1", "o2"), count = c(1L, 2L),
                   observationComments = c("fine", NA))
  issues <- line_break_schema()$validate(df, source = "observations.csv")
  expect_false(any(issues$constraint == "lineBreak"))
})
