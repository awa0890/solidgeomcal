#' Compute some key figures for a cylinder
#' @title Solid Geometry Calculator for cylinders
#'
#' @param radius The radius of the cylinder in millimeters.
#' @param height The height of the cylinder in millimeters.
#' @param unit The unit in which the key figures should be calculated. The User can select the input unit between millimeter ("mm"), centimeter ("cm") and meter ("m").
#'
#' @return Prints the diameter, scope, base area, lateral surface, surface and the volume of a cylinder for the inputs radius and height in the selected unit millimeter mm, centimeter cm or meter m.
#' @export
#'
#' @examples
#' calculate_cylinder(radius=50, height=20, unit = "mm")
#'
calculate_cylinder <- function(radius, height, unit = "mm") {
  # Error handling
  if(!is.numeric(radius)| !is.numeric(height)) {
    stop("radius or height is not numeric. \n Please check your inputs!")
  }
  if (!unit %in% c("mm", "cm", "m")) {
    stop("unit must be \"mm\", \"cm\" or \"m\".")
  }
  if(!is.character(unit)) {
    stop("unit must be a character. \n Please check your input!")
  }
  # Calculations
  if (unit == "cm") {
    radius_mm <- radius*10
    height_mm <- height*10
  } else if(unit == "m") {
    radius_mm <- radius*1000
    height_mm <- height*1000
  }
  else {
    radius_mm <- radius
    height_mm <- height
  }
  diameter <- 2 * radius_mm
  scope <- 2 * pi * radius_mm
  base_area <- pi * radius_mm^2
  surface_lateral <- 2 * pi * radius_mm * height_mm
  surface <- 2 * pi * radius_mm * (radius_mm + height_mm)
  volume <- pi * radius_mm^2 * height_mm

 # Output
  cat("Calculations for a cylinder with radius =", radius, "",unit," and height =", height, "",unit, ":\n")
  cat("------------------------------------------------------------\n")
  cat("Diameter:         ", diameter, "mm\n")
  cat("Scope :           ", round(scope, 2), "mm\n")
  cat("Base Area :       ", round(base_area, 2), "mm²\n")
  cat("Lateral Surface : ", round(surface_lateral, 2), "mm²\n")
  cat("Surface:          ", round(surface, 2), "mm²\n")
  cat("Volume:           ", round(volume, 2), "mm³\n")
}


