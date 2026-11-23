#' @title reportharvested_area_timber
#' @description reports MAgPIE harvested area for timber.
#'
#' @export
#'
#' @param gdx GDX file
#' @param level aggregation level of returned data ("regglo" by default)
#' @return Area harvested for timber production
#' @author Abhijeet Mishra
#' @examples
#'
#'   \dontrun{
#'     x <- reportharvested_area_timber(gdx)
#'   }
#'
#'
#' @section Timber harvested area variables:
#' Name | Unit | Meta
#' ---|---|---
#' Resources\|Timber operations\|Harvested area for timber\|Forestry | Mha per yr | Area harvested from managed forests
#' Resources\|Timber operations\|Harvested area for timber\|Primary forest | Mha per yr | Area harvested from primary forests
#' Resources\|Timber operations\|Harvested area for timber\|Secondary forest | Mha per yr | Area harvested from secondary forests
#' Resources\|Timber operations\|Selectively logged area | Mha per yr | Secondary forest selectively logged
#' @md


reportharvested_area_timber <- function(gdx, level = "regglo") {
  a <- NULL

  timber <- !is.null(readGDX(gdx, "s32_hvarea")) && !is.null(readGDX(gdx, "s35_hvarea"))

  if (timber) {
    a <- harvested_area_timber(gdx, level = level)
    getNames(a) <- paste0("Resources|Timber operations|Harvested area for timber|",
                          getNames(a))
    getNames(a) <- paste0(getNames(a), " (Mha per yr)")
    # standing area thinned in the time step (gdx files since the thinning of block T); not annualized
    thinSecd <- readGDX(gdx, "ov35_thin_secdforest", select = list(type = "level"), react = "silent")
    thinOpl <- readGDX(gdx, "ov32_thin_forestry", select = list(type = "level"), react = "silent")
    if (!is.null(thinSecd) && !is.null(thinOpl)) {
      thinned <- mbind(setNames(dimSums(thinSecd, dim = 3), "Secondary forest"), setNames(dimSums(thinOpl, dim = 3), "Other planted forest"))
      thinned <- mbind(thinned, setNames(dimSums(thinned, dim = 3), "Total"))
      thinned <- superAggregateX(thinned, aggr_type = "sum", level = level)
      getNames(thinned) <- paste0("Resources|Timber operations|Thinned area", c("|+|Secondary forest", "|+|Other planted forest", ""), " (Mha)")
      a <- mbind(a, thinned)
    }
    # secondary forest selectively logged per year (gdx files with selective logging)
    logged <- readGDX(gdx, "ov35_log_secdforest", select = list(type = "level"), react = "silent")
    if (!is.null(logged)) {
      periods <- timePeriods(gdx)
      periods[, 1, ] <- 5 # forestry time step of the first year in MAgPIE
      logged <- superAggregateX(dimSums(logged, dim = 3) / periods, aggr_type = "sum", level = level)
      a <- mbind(a, setNames(logged, "Resources|Timber operations|Selectively logged area (Mha per yr)"))
    }
  } else {
    message("Disabled (no timber) ", appendLF = FALSE)
  }

  return(a)
}
