
length <- function(value, from_unit, to_unit) {
  # Conversion factors relative to meter
  conversion_factors <- list(
    meter = 1,
    kilometer = 1 / 1000,
    decimeter = 10,
    decameter = 1 / 10,
    hectometer = 1 / 100,
    centimeter = 100,
    millimeter = 1000
  )

  # Check if units are recognized
  if (!(from_unit %in% names(conversion_factors)) || !(to_unit %in% names(conversion_factors))) {
    stop("Unknown units for length conversion")
  }

  # Convert to meters, then to the target unit
  value_in_base <- value / conversion_factors[[from_unit]] # Convert to meters
  result <- value_in_base * conversion_factors[[to_unit]] # Convert to target unit

  return(result)
}

# Function to Convert Time
#' Convert Time
#'
#' This function converts time values from one unit to another.
#'
#' @param value The time value to be converted.
#' @param from_unit The source unit (e.g., "second", "minute", "hour").
#' @param to_unit The target unit (e.g., "minute", "hour", "day").
#' @return The time value in the target unit.
#' @export
time <- function(value, from_unit, to_unit) {
  conversion_factors <- list(
    second = 1,
    minute = 1 / 60,
    hour = 1 / 3600,
    day = 1 / 86400,
    millisecond = 1000
  )

  # Check if units are recognized
  if (!(from_unit %in% names(conversion_factors)) || !(to_unit %in% names(conversion_factors))) {
    stop("Unknown units for time conversion")
  }

  # Convert to seconds, then to the target unit
  value_in_seconds <- value / conversion_factors[[from_unit]]
  result <- value_in_seconds * conversion_factors[[to_unit]]
  return(result)
}

# Function to Convert Temperature
#' Convert Temperature
#'
#' This function converts temperature values from one unit to another.
#'
#' @param value The temperature value to be converted.
#' @param from_unit The source unit (e.g., "celsius", "fahrenheit", "reamur").
#' @param to_unit The target unit (e.g., "fahrenheit", "kelvin", "reamur").
#' @return The temperature value in the target unit.
#' @export
temp <- function(value, from_unit, to_unit) {
  if (from_unit == "celsius" && to_unit == "fahrenheit") {
    return((value * 9/5) + 32)
  } else if (from_unit == "fahrenheit" && to_unit == "celsius") {
    return((value - 32) * 5/9)
  } else if (from_unit == "celsius" && to_unit == "kelvin") {
    return(value + 273.15)
  } else if (from_unit == "kelvin" && to_unit == "celsius") {
    return(value - 273.15)
  } else if (from_unit == "celsius" && to_unit == "reamur") {
    return(value * 4/5)
  } else if (from_unit == "reamur" && to_unit == "celsius") {
    return(value * 5/4)
  } else if (from_unit == "reamur" && to_unit == "fahrenheit") {
    return((value * 9/4) + 32)
  } else if (from_unit == "fahrenheit" && to_unit == "reamur") {
    return((value - 32) * 4/9)
  } else if (from_unit == "reamur" && to_unit == "kelvin") {
    return((value * 5/4) + 273.15)
  } else if (from_unit == "kelvin" && to_unit == "reamur") {
    return((value - 273.15) * 4/5)
  } else if (from_unit == "fahrenheit" && to_unit == "kelvin") {
    return((value - 32) * 5/9 + 273.15)
  } else if (from_unit == "kelvin" && to_unit == "fahrenheit") {
    return((value - 273.15) * 9/5 + 32)
  } else {
    stop("Unknown units for temperature conversion")
  }
}

# Function to Convert Mass
#' Convert Mass
#'
#' This function converts mass values from one unit to another.
#'
#' @param value The mass value to be converted.
#' @param from_unit The source unit (e.g., "gram", "kilogram", "milligram").
#' @param to_unit The target unit (e.g., "kilogram", "ton", "quintal").
#' @return The mass value in the target unit.
#' @export
mass <- function(value, from_unit, to_unit) {
  # Conversion factors relative to gram
  conversion_factors <- list(
    gram = 1,
    kilogram = 1 / 1000,
    milligram = 1000,
    ton = 1 / 1e6,
    quintal = 1 / 1e5
  )

  # Check if units are recognized
  if (!(from_unit %in% names(conversion_factors)) || !(to_unit %in% names(conversion_factors))) {
    stop("Unknown units for mass conversion")
  }

  # Convert to grams, then to the target unit
  value_in_grams <- value / conversion_factors[[from_unit]] # Convert to grams
  result <- value_in_grams * conversion_factors[[to_unit]] # Convert to target unit

  return(result)
}

# Function to Convert Data Storage
#' Convert Data Storage
#'
#' This function converts data storage values from one unit to another.
#'
#' @param value The data storage value to be converted.
#' @param from_unit The source unit (e.g., "bit", "byte", "kilobyte").
#' @param to_unit The target unit (e.g., "megabyte", "gigabyte").
#' @return The data storage value in the target unit.
#' @export
storage <- function(value, from_unit, to_unit) {
  # Conversion factors for data storage
  conversion_factors <- list(
    bit = 1,
    byte = 8,
    kilobyte = 1024,
    megabyte = 1024^2,
    gigabyte = 1024^3,
    terabyte = 1024^4,
    petabyte = 1024^5,
    exabyte = 1024^6,
    zettabyte = 1024^7,
    yottabyte = 1024^8,
    brontobyte = 1024^9
  )

  # Check if units are recognized
  if (!(from_unit %in% names(conversion_factors)) || !(to_unit %in% names(conversion_factors))) {
    stop("Unknown units for storage conversion")
  }

  # Convert value to bits as the base unit
  value_in_bits <- value * conversion_factors[[from_unit]]

  # Convert value to the target unit
  result <- value_in_bits / conversion_factors[[to_unit]]

  return(result)
}
