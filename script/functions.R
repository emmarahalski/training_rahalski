

interior_f <- c(175, 134, 181)
interior_c <- (interior_f - 32) * 5/9

exterior_f <- c(77, 78, 77)
exterior_c <- (exterior_f - 32) * 5/9

surface_f <- c(103, 102, 99)
surface_c <- (surface_f - 32) * 5/9



convert_f_to_c <-  function(fahr) {
  celsius <- (fahr - 32) * 5/9
  return(celsius)
}

surface_c_v2 <- convert_f_to_c(fahr = surface_f)
surface_c_v2

#Check to see if they match
identical(surface_c_v2, surface_c)

convert_temps <- function(fahr) {
  celsius <- (fahr - 32) * 5/9
  kelvin <- celsius + 273.15
  return(list(fahr = fahr, celsius = celsius, kelvin = kelvin))
}

surface_temps_df <- data.frame(convert_temps(fahr = surface_f))


convert_temps2 <- function(temp, unit = "F") {
  
  ### Error checking:
  unit <- toupper(unit)  ## try to anticipate common mistakes!
  if (!unit %in% c("F", "C")) {
    stop("The units must be either F or C!")
  }
  
  if (unit == "F") {
    fahr <- temp
    celsius <- (fahr - 32) * 5/9
  } else {
    celsius <- temp
    fahr <- celsius * 9 / 5 + 32
  }
  kelvin <- celsius + 273.15
  
  out_df <- data.frame(fahr, celsius, kelvin)
  return(out_df)
}

# run on the Celsius values, using named arguments
surface_temps_df1 <- convert_temps2(temp = surface_c, unit = "C")

# run on the Fahrenheit values, using positional arguments
surface_temps_df2 <- convert_temps2(surface_f, "F")

# run on the Fahrenheit values, using default `unit` of "F"
surface_temps_df3 <- convert_temps2(surface_f)

# check that the last output matches our earlier data frame
identical(surface_temps_df, surface_temps_df3)
