#' Read Geolocator Data and Orders
#'
#' Read and combine Swiss Ornithological Institute GDL exports.
#'
#' `read_soi_gld()` is the high-level entry point. It can read from:
#' - a single Access (`.accdb`) file, or
#' - separate data/order CSV files.
#'
#' The result is a joined table combining tag data with summarized order metadata.
#'
#' @param access_file A string specifying the path to an Access file containing both the GDL data
#' and order information. If provided, it takes precedence over `data_file` and
#' `order_file`. Defaults to `NA`.
#' @param data_file A string specifying the path to the GDL data file. Required if `access_file`
#' is not provided. Defaults to `NA`.
#' @param order_file A string specifying the path to the GDL order file. Required if `access_file`
#' is not provided. Defaults to `NA`.
#' @param filter_col A logical value or a character vector. If `TRUE`, only a predefined set of
#' columns is selected.
#' If `FALSE`, all columns are included. Alternatively, a character vector can be passed to specify
#' which columns to select. Defaults to `TRUE`.
#'
#' @return A data frame combining GDL tag rows and order-level information.
#'
#' @details
#' Helper functions:
#'
#' - **`read_soi_gld()`**: Reads GDL and order data, either from an Access file or separate CSV files,
#' and returns a combined data frame.
#' - **`read_soi_gld_orders()`**: Reads order metadata from a CSV export.
#' - **`read_soi_gld_data()`**: Reads GDL tag-level data from a CSV export.
#' - **`read_soi_gld_access()`**: Extracts GDL data and order information from an Access database file
#' and exports them to temporary CSV files.
#'
#' @noRd
read_soi_gld <- function(
  access_file = NA,
  data_file = NA,
  order_file = NA,
  filter_col = TRUE
) {
  if (!is.na(access_file)) {
    if (!is.na(data_file) || !is.na(order_file)) {
      cli_warn(c(
        "!" = "Both {.arg access_file} and {.arg data_file} and {.arg order_file} were provided.",
        ">" = "We'll use {.arg access_file}."
      ))
    }
    data_order_file <- read_soi_gld_access(access_file)
    data_file <- data_order_file[[1]]
    order_file <- data_order_file[[2]]
  } else {
    if (is.na(data_file) || is.na(order_file)) {
      cli_abort(c(
        "x" = "Need {.arg access_file} or both {.arg data_file} and {.arg order_file}."
      ))
    }
  }

  if (is.data.frame(data_file)) {
    d <- data_file
  } else {
    d <- read_soi_gld_data(data_file)
  }

  if (is.data.frame(order_file)) {
    o <- order_file
  } else {
    o <- read_soi_gld_orders(order_file)
  }

  o <- o |>
    group_by(.data$OrderName) |>
    summarize(
      NumberOrdered = sum(.data$NumberOrdered),
      NumberDelivered = sum(.data$NumberDelivered),
      across(
        where(~ !is.numeric(.)),
        ~ paste(unique(.), collapse = ", ") # Concatenate unique values for non-numeric columns
      )
    ) |>
    select(-c("GDL_Type", "Species"))

  gdl <- left_join(d, o, by = c("OrderName"), suffix = c("_data", "_order"))

  if (is.logical(filter_col)) {
    if (filter_col) {
      gdl <- select(
        gdl,
        c(
          # Order
          "OrderName",
          "Client",
          "ResponsibleP3",
          "ClientCategory",
          "NumberDelivered",
          "RemarksProduction",
          "RemarksOrder",
          "Remarks",

          # GDL
          "GDL_ID",
          "GDL_Type",
          "Species",
          "UTC_Attached",
          "SiteAttached",
          "LongitudeAttached",
          "LatitudeAttached",
          "UTC_Removed",
          "LongitudeRemoved",
          "LongitudeRemoved",
          "LongitudeAttached",

          # Information needed for package
          "RingNumber",
          "FirmwareVersion",
          "HardwareVersion",
          "TotalWeight"
          # "Harness_data","BreastHarnessDiameterHead",...
        )
      )
    }
  } else {
    gdl <- select(gdl, "filter_col")
  }

  return(gdl)
}

#' @noRd
read_soi_gld_orders <- function(order_file) {
  o <- readr::read_csv(
    order_file,
    col_types = readr::cols(
      OrderID = readr::col_integer(),
      OrderName = readr::col_character(),
      Species = readr::col_character(),
      Client = readr::col_character(),
      ResponsibleP3 = readr::col_character(),
      ClientCategory = readr::col_factor(c(
        "own",
        "cooperation",
        "coop",
        "purchase"
      )),
      GDL_Type = readr::col_character(),
      # readr::col_factor(c("GDL1", "GDL2", "GDL3pam", "uTag")),
      NumberOrdered = readr::col_integer(),
      OrderStatus = readr::col_factor(c("ordered", "delivered", "request")),
      NumberDelivered = readr::col_integer(),
      DeliveryDate = readr::col_datetime(),
      DateDelivered = readr::col_datetime(),
      UnitPrice = readr::col_double(),
      Currency = readr::col_factor(c("CHF")),
      InvoiceDate = readr::col_datetime(),
      RemarksProduction = readr::col_character(),
      RemarksOrder = readr::col_character(),
      IntervalBlue = readr::col_integer(),
      LoggingPeriodEnabled = readr::col_logical(),
      StartPeriod1 = readr::col_datetime(),
      IntervalRGC = readr::col_integer(),
      GreenEnabled = readr::col_logical(),
      RedEnabled = readr::col_logical(),
      ClearEnabled = readr::col_logical(),
      LightLevelThreshold = readr::col_integer(),
      BaseLoggingInterval = readr::col_integer(),
      IntervalFactorLight = readr::col_integer(),
      IntervalFactorAirTemperature = readr::col_integer(),
      IntervalFactorBodyTemperature = readr::col_integer(),
      IntervalFactorPressure = readr::col_integer(),
      IntervalFactorAcceleration = readr::col_integer(),
      IntervalFactorMagnetic = readr::col_integer(),
      LWL = readr::col_logical(),
      LWL_Length = readr::col_integer(),
      LWL_Diameter = readr::col_integer(),
      Harness = readr::col_character(),
      HarnessMaterial = readr::col_character(),
      HarnessAttachement = readr::col_character(),
      HarnessThickness = readr::col_double(),
      LegHarnessDiameter = readr::col_double(),
      BreastHarnessDiameterHead = readr::col_double(),
      BreastHarnessDiameterTail = readr::col_double(),
      HarnessDescription = readr::col_character(),
      AccSamplingRate = readr::col_logical(),
      AccXaxisEnabled = readr::col_logical(),
      AccYaxisEnabled = readr::col_logical(),
      AccZaxisEnabled = readr::col_logical(),
      DataloggerEnabled = readr::col_logical(),
      RadioEnabled = readr::col_logical(),
      Frequencies = readr::col_character(),
      TransmitPower = readr::col_double(),
      WakeUpTrigger = readr::col_double(),
      PatternID = readr::col_double(),
      PulseWidth = readr::col_double(),
      PulseInterval = readr::col_double(),
      PatternInterval = readr::col_double(),
      PulsePattern = readr::col_character(),
      TRSchedule = readr::col_character(),
      LoggingSchedule = readr::col_character(),
      ActiveAfterWakeUp = readr::col_double(),
      PrioritySensor = readr::col_logical(),
      PriorityMemory = readr::col_double()
    )
  ) |>
    arrange(.data$OrderName)
  o
}


#' @noRd
read_soi_gld_data <- function(data_file) {
  d <- readr::read_csv(
    data_file,
    col_types = readr::cols(
      DataID = readr::col_integer(),
      OrderName = readr::col_character(),
      GDL_ID = readr::col_character(),
      RingNumber = readr::col_character(),
      Species = readr::col_character(),
      SiteAttached = readr::col_character(),
      UTC_Attached = readr::col_datetime(),
      LongitudeAttached = readr::col_double(),
      LatitudeAttached = readr::col_double(),
      DateDeparture = readr::col_datetime(),
      DateArrival = readr::col_datetime(),
      UTC_Removed = readr::col_datetime(),
      LatitudeRemoved = readr::col_double(),
      DateDownload = readr::col_datetime(),
      UTC_StartRTC = readr::col_datetime(),
      UTC_StopRTC = readr::col_datetime(),
      UTC_StopReference = readr::col_datetime(),
      UTC_FirstData = readr::col_datetime(),
      UTC_LastData = readr::col_datetime(),
      DataFileName = readr::col_character(),
      RooftopCalFileName = readr::col_character(),
      UTC_StartRooftopCal = readr::col_datetime(),
      UTC_StopRooftopCal = readr::col_datetime(),
      LongitudeRooftopCal = readr::col_double(),
      LatitudeRooftopCal = readr::col_double(),
      UTC_StartStatCal1 = readr::col_datetime(),
      UTC_StopStatCal1 = readr::col_datetime(),
      LongitudeStatCal1 = readr::col_double(),
      LatitudeStatCal1 = readr::col_double(),
      UTC_StartStatCal2 = readr::col_datetime(),
      UTC_StopStatCal2 = readr::col_datetime(),
      LongitudeStatCal2 = readr::col_double(),
      LatitudeStatCal2 = readr::col_double(),
      NightBaseline = readr::col_double(),
      LevelThreshold = readr::col_double(),
      GDL_Type = readr::col_character(),
      HardwareVersion = readr::col_character(),
      FirmwareVersion = readr::col_character(),
      IntervalBlue = readr::col_integer(),
      LoggingPeriodEnabled = readr::col_logical(),
      UTC_StartLog1 = readr::col_datetime(),
      IntervalRGC = readr::col_double(),
      GreenEnabled = readr::col_logical(),
      RedEnabled = readr::col_logical(),
      ClearEnabled = readr::col_logical(),
      LightLevelThreshold = readr::col_double(),
      BaseLoggingInterval = readr::col_integer(),
      IntervalFactorLight = readr::col_integer(),
      IntervalFactorAirTemperature = readr::col_integer(),
      IntervalFactorBodyTemperature = readr::col_integer(),
      IntervalFactorPressure = readr::col_integer(),
      IntervalFactorAcceleration = readr::col_integer(),
      IntervalFactorMagnetic = readr::col_integer(),
      LWL = readr::col_double(),
      LWL_Length = readr::col_double(),
      LWL_Diameter = readr::col_double(),
      DateBatteryConnected = readr::col_character(),
      VoltageReady = readr::col_double(),
      VoltageEnd = readr::col_double(),
      TotalWeight = readr::col_double(),
      MemoryUsed = readr::col_double(),
      Harness = readr::col_character(),
      HarnessMaterial = readr::col_character(),
      HarnessAttachement = readr::col_character(),
      HarnessThickness = readr::col_double(),
      LegHarnessDiameter = readr::col_double(),
      BreastHarnessDiameterHead = readr::col_double(),
      BreastHarnessDiameterTail = readr::col_double(),
      PrintManufacturer = readr::col_character(),
      FinalAssembly = readr::col_character(),
      Remarks = readr::col_character(),
      Frequency = readr::col_double(),
      MagCalib = readr::col_character(),
      SleepCurrent = readr::col_double(),
      PrioritySensor = readr::col_logical(),
      PriorityMemory = readr::col_logical(),
      LongitudeRemoved = readr::col_double()
    )
  )

  d
}

#' @noRd
read_soi_gld_access <- function(
  access_file,
  data_file = tempfile("GDL_Data", fileext = ".csv"),
  order_file = tempfile("GDL_Orders", fileext = ".csv")
) {
  # Check Access file
  if (!file.exists(access_file)) {
    cli_abort(c(
      "x" = "The access file {.file {access_file}} does not exist."
    ))
  }
  if (tolower(tools::file_ext(access_file)) != "accdb") {
    cli_abort(c(
      "x" = "The access file {.file {access_file}} does not have a {.val .accdb} extension."
    ))
  }

  if (.Platform$OS.type == "unix") {
    # Export data
    system(glue::glue("mdb-export '{access_file}' GDL_Data > {data_file}"))

    if (!file.exists(data_file)) {
      cli_abort(c(
        "x" = "The file {.file {data_file}} does not exist.",
        "i" = "There has been an issue in creating the data file."
      ))
    }

    # Export orders
    system(glue::glue("mdb-export '{access_file}' GDL_Orders > {order_file}"))
    if (!file.exists(order_file)) {
      cli_abort(c(
        "x" = "The file {.file {order_file}} does not exist.",
        "i" = "There has been an issue in creating the order file."
      ))
    }
  } else if (.Platform$OS.type == "windows") {
    if (
      !requireNamespace("DBI", quietly = TRUE) ||
        !requireNamespace("odbc", quietly = TRUE)
    ) {
      cli_abort(
        "The {.pkg DBI} and {.pkg odbc} packages are required to read Access files on Windows."
      )
    }
    con <- DBI::dbConnect(
      odbc::odbc(),
      .connection_string = paste0(
        "Driver={Microsoft Access Driver (*.mdb, *.accdb)};",
        "Dbq=",
        access_file,
        ";"
      )
    )
    readr::write_csv(DBI::dbReadTable(con, "GDL_Orders"), order_file)
    readr::write_csv(DBI::dbReadTable(con, "GDL_Data"), data_file)
  }

  invisible(c(data_file, order_file))
}
