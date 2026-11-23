#' @title reportGrowingStock
#' @description reports Growing stocks for woody materials
#'
#' @import magpiesets
#' @export
#'
#' @param gdx GDX file
#' @param level aggregation level of returned data ("regglo" by default)
#' @param indicator If the reported numbers are relative (mio m3/ha) or absolute (mio. m3). Default is relative.
#' @param detail if detail=FALSE, the subcategories of groups are not reported.
#' @return production as MAgPIE object. Unit: see names
#' @author Abhijeet Mishra
#' @examples
#'
#'   \dontrun{
#'     x <- reportGrowingStock(gdx)
#'   }
#'
#' @section Growing stock variables:
#' Name | Unit | Meta
#' ---|---|---
#' Resources\|Growing Stock\|relative\|Forest | m3/ha | Relative growing stock in forests
#' Resources\|Growing Stock\|relative\|Forest excl CO2-price AR | m3/ha | Relative growing stock in forests without CO2-price afforestation (comparable to FRA)
#' Resources\|Growing Stock\|relative\|CO2-price AR | m3/ha | Relative growing stock in CO2-price afforestation
#' Resources\|Growing Stock\|relative\|NPI_NDC AR | m3/ha | Relative growing stock in NPI/NDC afforestation
#' Resources\|Growing Stock\|relative\|Plantations | m3/ha | Relative growing stock in plantations
#' Resources\|Growing Stock\|relative\|Other Planted Forest | m3/ha | Relative growing stock in other planted forest (gdx with that pool)
#' Resources\|Growing Stock\|absolute\|Forest | Mm3 | Absolute growing stock in forests
#' Resources\|Growing Stock\|absolute\|Plantations | Mm3 | Absolute growing stock in plantations
#' Resources\|Growing Stock\|absolute\|Other Planted Forest | Mm3 | Absolute growing stock in other planted forest (gdx with that pool)
#' @md

reportGrowingStock <- function(gdx, indicator = "relative", detail = FALSE, level = "regglo") {
  if (suppressWarnings(!is.null(readGDX(gdx, "fcostsALL")))) {
    x <- GrowingStock(gdx = gdx, level = level, indicator = indicator)
    if (indicator == "relative") {
      unit <- "(m3/ha)"
    } else if (indicator == "absolute") {
      unit <- "(Mm3)"
    }
    getNames(x) <- suppressWarnings(paste0("Resources|Growing Stock|", indicator, "|", reportingnames(getNames(x, dim = 1))))
    # the FRA category name of the other planted forest pool, which magpiesets does not map
    getNames(x) <- sub("|other_planted", "|Other Planted Forest", getNames(x), fixed = TRUE)
    # afforestation under the planted-forest names of the land reporting
    getNames(x) <- sub("\\|aff$", "|CO2-price AR", getNames(x))
    getNames(x) <- sub("\\|ndc$", "|NPI_NDC AR", getNames(x))
    getNames(x) <- sub("|forest_excl_aff", "|Forest excl CO2-price AR", getNames(x), fixed = TRUE)
    getNames(x) <- paste(getNames(x), unit, sep = " ")
    # the forest total without CO2-price AR is no summand, so it stays outside the summation markers
    excl <- grep("|Forest excl CO2-price AR", getNames(x), fixed = TRUE, value = TRUE)
    x <- mbind(summationhelper(x[, , excl, invert = TRUE]), x[, , excl])
    return(x)
  } else {
    return(NULL)
  }
}
