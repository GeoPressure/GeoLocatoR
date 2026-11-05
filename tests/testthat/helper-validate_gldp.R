make_geopressure_test_pkg <- function() {
  pkg <- create_gldp()
  start <- as.POSIXct("2024-01-01 00:00:00", tz = "UTC")
  end <- as.POSIXct("2024-01-01 06:00:00", tz = "UTC")

  # Minimal in-memory resources to exercise range and FK coherence checks.
  pkg[["resources"]] <- list(
    list(
      name = "tags",
      data = data.frame(tag_id = "tag-1", stringsAsFactors = FALSE),
      schema = list()
    ),
    list(
      name = "staps",
      data = data.frame(
        tag_id = "tag-1",
        stap_id = 1,
        start = start,
        end = end,
        stringsAsFactors = FALSE
      ),
      schema = list(
        foreignKeys = list(
          list(
            fields = "tag_id",
            reference = list(resource = "tags", fields = "tag_id")
          )
        )
      )
    ),
    list(
      name = "twilights",
      data = data.frame(
        tag_id = "tag-1",
        twilight = start + 1800,
        stringsAsFactors = FALSE
      ),
      schema = list(
        foreignKeys = list(
          list(
            fields = "tag_id",
            reference = list(resource = "tags", fields = "tag_id")
          )
        )
      )
    ),
    list(
      name = "paths",
      data = data.frame(
        tag_id = "tag-1",
        type = "most_likely",
        stap_id = 1,
        lat = 45,
        lon = 8,
        j = 1L,
        stringsAsFactors = FALSE
      ),
      schema = list(
        foreignKeys = list(
          list(
            fields = "tag_id",
            reference = list(resource = "tags", fields = "tag_id")
          ),
          list(
            fields = c("tag_id", "stap_id"),
            reference = list(resource = "staps", fields = c("tag_id", "stap_id"))
          )
        )
      )
    ),
    list(
      name = "edges",
      data = data.frame(
        tag_id = "tag-1",
        type = "most_likely",
        stap_s = 1,
        stap_t = 1,
        lat_s = 45,
        lon_s = 8,
        lat_t = 46,
        lon_t = 9,
        j = 1L,
        start = start,
        end = start + 3600,
        n = 1L,
        distance = 100,
        gs_u = 5,
        gs_v = 5,
        ws_u = 2,
        ws_v = 2,
        stringsAsFactors = FALSE
      ),
      schema = list(
        foreignKeys = list(
          list(
            fields = "tag_id",
            reference = list(resource = "tags", fields = "tag_id")
          ),
          list(
            fields = c("tag_id", "stap_s"),
            reference = list(resource = "staps", fields = c("tag_id", "stap_id"))
          ),
          list(
            fields = c("tag_id", "stap_t"),
            reference = list(resource = "staps", fields = c("tag_id", "stap_id"))
          )
        )
      )
    ),
    list(
      name = "pressurepaths",
      data = data.frame(
        tag_id = "tag-1",
        datetime = start + 600,
        stap_id = 1,
        type = "most_likely",
        lat = 45,
        lon = 8,
        j = 1L,
        altitude = 100,
        label = NA_character_,
        stringsAsFactors = FALSE
      ),
      schema = list(
        foreignKeys = list(
          list(
            fields = "tag_id",
            reference = list(resource = "tags", fields = "tag_id")
          )
        )
      )
    )
  )

  pkg
}
