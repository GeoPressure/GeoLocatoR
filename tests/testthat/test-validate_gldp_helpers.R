library(testthat)
library(GeoLocatoR)

test_that("check_type function validates basic types correctly", {
  # String types
  suppressMessages({
    expect_true(check_type("hello", "string", "test_field"))
    expect_true(check_type(c("hello", "world"), "string", "test_field"))
    expect_false(check_type(123, "string", "test_field"))

    # Number types
    expect_true(check_type(123, "number", "test_field"))
    expect_true(check_type(123.456, "number", "test_field"))
    expect_false(check_type("123", "number", "test_field"))

    # Integer types
    expect_true(check_type(123L, "integer", "test_field"))
    expect_true(check_type(123, "integer", "test_field")) # numeric whole number
    expect_false(check_type(123.456, "integer", "test_field"))

    # Boolean types
    expect_true(check_type(TRUE, "boolean", "test_field"))
    expect_true(check_type(c(TRUE, FALSE), "boolean", "test_field"))
    expect_false(check_type("true", "boolean", "test_field"))

    # Object types
    expect_true(check_type(list(a = 1, b = 2), "object", "test_field"))
    expect_false(check_type(data.frame(a = 1), "object", "test_field"))

    # Array types
    expect_true(check_type(c(1, 2, 3), "array", "test_field"))
    expect_true(check_type(list(1, 2, 3), "array", "test_field"))

    # Null type
    expect_true(check_type(NULL, "null", "test_field"))
    expect_false(check_type(NA, "null", "test_field"))

    # Any type accepts everything
    expect_true(check_type("anything", "any", "test_field"))
    expect_true(check_type(123, "any", "test_field"))

    # Unknown type
    expect_false(check_type("value", "unknown_type", "test_field"))

    # NULL type parameter returns TRUE
    expect_true(check_type("anything", NULL, "test_field"))
  })
})

test_that("check_type function validates datetime types correctly", {
  suppressMessages({
    # Valid datetime objects
    expect_true(check_type(Sys.time(), "datetime", "test_field"))
    expect_false(check_type(Sys.Date(), "datetime", "test_field"))

    # Valid datetime strings with Z suffix
    expect_true(check_type("2023-12-25T10:30:45Z", "datetime", "test_field"))
    datetime_values <- c("2023-12-25T10:30:45Z", "2024-01-01T00:00:00Z")
    expect_true(check_type(datetime_values, "datetime", "test_field"))

    # Invalid datetime strings (missing Z)
    expect_false(check_type("2023-12-25T10:30:45", "datetime", "test_field"))
    expect_false(check_type("invalid-date", "datetime", "test_field"))
  })
})

test_that("check_type function validates specialized types correctly", {
  suppressMessages({
    # Year types
    expect_true(check_type(2023, "year", "test_field"))
    expect_true(check_type(c(2023, 2024), "year", "test_field"))
    expect_true(check_type("2023", "year", "test_field"))
    expect_false(check_type(23, "year", "test_field")) # too short
    expect_false(check_type(2023.5, "year", "test_field")) # not integer

    # Yearmonth types
    expect_true(check_type("2023-12", "yearmonth", "test_field"))
    expect_false(check_type("2023/12", "yearmonth", "test_field"))
    expect_false(check_type("2023-1", "yearmonth", "test_field")) # single digit month

    # Duration types
    expect_true(check_type("P1Y", "duration", "test_field"))
    expect_true(check_type("P1M", "duration", "test_field"))
    expect_true(check_type("PT1H", "duration", "test_field"))
    expect_false(check_type("1Y", "duration", "test_field")) # missing P
    expect_false(check_type("P", "duration", "test_field")) # no components

    # Geopoint types
    expect_true(check_type("45.123, -122.456", "geopoint", "test_field"))
    expect_true(check_type("0, 0", "geopoint", "test_field"))
    expect_false(check_type("91, 0", "geopoint", "test_field")) # lat out of range
    expect_false(check_type("45.123", "geopoint", "test_field")) # missing longitude

    # GeoJSON types
    geojson_geometry <- '{"type": "Point", "coordinates": [0, 0]}'
    valid_geojson <- sprintf(
      '{"type": "Feature", "geometry": %s, "properties": {}}',
      geojson_geometry
    )
    expect_true(check_type(valid_geojson, "geojson", "test_field"))
    expect_false(check_type('{"type": "NotFeature"}', "geojson", "test_field"))
    expect_false(check_type("not json", "geojson", "test_field"))
  })
})

test_that("check_format function validates basic formats correctly", {
  suppressMessages({
    # Date-time format
    expect_true(check_format("2023-12-25T10:30:45Z", "date-time", "test_field"))
    expect_false(check_format("2023-12-25T10:30:45", "date-time", "test_field")) # missing Z

    # Date format
    expect_true(check_format("2023-12-25", "date", "test_field"))
    expect_false(check_format("2023/12/25", "date", "test_field"))

    # Time format
    expect_true(check_format("10:30:45", "time", "test_field"))
    expect_false(check_format("10:30", "time", "test_field")) # missing seconds

    # UTC milliseconds
    expect_true(check_format(1640431845000, "utc-millisec", "test_field"))
    expect_false(check_format("1640431845000", "utc-millisec", "test_field")) # string

    # Colors
    expect_true(check_format("#FF0000", "color", "test_field"))
    expect_true(check_format("red", "color", "test_field"))
    expect_false(check_format("purple", "color", "test_field")) # not in allowed names

    # Email addresses
    expect_true(check_format("user@example.com", "email", "test_field"))
    expect_false(check_format("invalid-email", "email", "test_field"))
    expect_false(check_format("user@domain", "email", "test_field")) # missing TLD

    # Unknown format
    expect_false(check_format("value", "unknown_format", "test_field"))

    # NULL format parameter
    expect_true(check_format("anything", NULL, "test_field"))
  })
})

test_that("check_format and check_type work with vectors", {
  suppressMessages({
    # Test with vectors of valid values
    datetime_vector <- c("2023-12-25T10:30:45Z", "2024-01-01T00:00:00Z")
    expect_true(check_type(datetime_vector, "datetime", "test_field"))
    expect_true(check_format(c("2023-12-25", "2024-01-01"), "date", "test_field"))

    # Test with vectors containing some invalid values
    expect_false(check_type(c("2023-12-25T10:30:45Z", "invalid"), "datetime", "test_field"))
    expect_false(check_format(c("2023-12-25", "invalid-date"), "date", "test_field"))
  })
})

test_that("check_type and check_format handle NA values correctly", {
  suppressMessages({
    # NA values should be handled gracefully
    expect_true(check_type(c("hello", NA, "world"), "string", "test_field"))
    expect_true(check_format(c("2023-12-25", NA, "2024-01-01"), "date", "test_field"))

    # Mixed valid/invalid with NA
    expect_false(check_type(c("2023-12-25T10:30:45Z", NA, "invalid"), "datetime", "test_field"))
    expect_false(check_format(c("2023-12-25", NA, "invalid"), "date", "test_field"))
  })
})
