#' @title reportTimber
#' @description reports MAgPIE demand for timber.
#'
#' @export
#'
#' @param gdx GDX file
#' @param level aggregation level of returned data ("regglo" by default)
#' @return Timber demand
#' @author Abhijeet Mishra
#' @examples
#'
#'   \dontrun{
#'     x <- reportTimber(gdx)
#'   }
#'
#'
#' @section Timber demand variables:
#' Name | Unit | Meta
#' ---|---|---
#' Timber\|Volumetric\|Demand\|+\|Roundwood | Mm3/yr | Total roundwood demand
#' Timber\|Volumetric\|Demand\|Roundwood\|+\|Industrial roundwood | Mm3/yr | Demand for industrial roundwood
#' Timber\|Volumetric\|Demand\|Roundwood\|+\|Wood fuel | Mm3/yr | Demand for wood fuel
#' Timber\|Volumetric\|Demand\|Roundwood\|+\|Timber for construction | Mm3/yr | Demand for timber used in construction
#'
#' @section Timber production variables:
#' Name | Unit | Meta
#' ---|---|---
#' Timber\|Volumetric\|Production\|+\|Roundwood | Mm3/yr | Total roundwood production
#' Timber\|Volumetric\|Production\|Roundwood\|+\|Industrial roundwood | Mm3/yr | Production of industrial roundwood
#' Timber\|Volumetric\|Production\|Roundwood\|+\|Wood fuel | Mm3/yr | Production of wood fuel
#' Timber\|Volumetric\|Production\|Roundwood\|+\|Timber for construction | Mm3/yr | Production of timber for construction
#' Timber\|Volumetric\|Production\|Roundwood\|Selective logging | Mm3/yr | Felled stem volume of selectively logged secondary forest
#'
#' @section Timber trade variables:
#' Name | Unit | Meta
#' ---|---|---
#' Timber\|Volumetric\|Net-Trade\|+\|Roundwood | Mm3/yr | Net export of roundwood
#' Timber\|Volumetric\|Net-Trade\|Roundwood\|+\|Industrial roundwood | Mm3/yr | Net export of industrial roundwood
#' Timber\|Volumetric\|Net-Trade\|Roundwood\|+\|Wood fuel | Mm3/yr | Net export of wood fuel
#' Timber\|Volumetric\|Net-Trade\|Roundwood\|+\|Timber for construction | Mm3/yr | Net export of timber for construction
#' Timber\|Volumetric\|Exports\|+\|Roundwood | Mm3/yr | Gross exports of roundwood
#' Timber\|Volumetric\|Exports\|Roundwood\|+\|Industrial roundwood | Mm3/yr | Gross exports of industrial roundwood
#' Timber\|Volumetric\|Exports\|Roundwood\|+\|Wood fuel | Mm3/yr | Gross exports of wood fuel
#' Timber\|Volumetric\|Exports\|Roundwood\|+\|Timber for construction | Mm3/yr | Gross exports of timber for construction
#' Timber\|Volumetric\|Imports\|+\|Roundwood | Mm3/yr | Gross imports of roundwood
#' Timber\|Volumetric\|Imports\|Roundwood\|+\|Industrial roundwood | Mm3/yr | Gross imports of industrial roundwood
#' Timber\|Volumetric\|Imports\|Roundwood\|+\|Wood fuel | Mm3/yr | Gross imports of wood fuel
#' Timber\|Volumetric\|Imports\|Roundwood\|+\|Timber for construction | Mm3/yr | Gross imports of timber for construction
#' @md


reportTimber <- function(gdx, level = "regglo") {
  if (suppressWarnings(!is.null(readGDX(gdx, "fcostsALL")))) {
    a <- Timber(gdx, level = level)
    getNames(a, dim = 2) <- reportingnames(getNames(a, dim = 2))

    dem <- collapseNames(a[, , "Demand"])
    getNames(dem) <- paste0("Timber|Volumetric|Demand|Roundwood|+|", getNames(dem))
    dem <- mbind(setNames(dimSums(dem, dim = 3), "Timber|Volumetric|Demand|+|Roundwood"),
                 dem)
    getNames(dem) <- paste0(getNames(dem), " (Mm3/yr)")

    prod <- collapseNames(a[, , "Production"])
    getNames(prod) <- paste0("Timber|Volumetric|Production|Roundwood|+|", getNames(prod))
    prod <- mbind(setNames(dimSums(prod, dim = 3), "Timber|Volumetric|Production|+|Roundwood"),
                  prod)
    getNames(prod) <- paste0(getNames(prod), " (Mm3/yr)")

    # production by source, thinning wood and sawlog-grade wood; older gdx files lack the thinning and sawlog variables
    lvl <- list(type = "level")
    density <- readGDX(gdx, "im_vol_conv", "f73_volumetric_conversion", react = "silent", format = "first_found")
    toVol <- function(x, nm) {
      if (is.null(x)) return(NULL)
      x <- gdxAggregate(gdx, dimSums(x, dim = 3) / density, to = level, absolute = TRUE)
      setNames(x, paste0(nm, " (Mm3/yr)"))
    }
    bind <- function(...) {
      l <- Filter(Negate(is.null), list(...))
      if (length(l) > 0) do.call(mbind, l) else NULL
    }
    natveg <- readGDX(gdx, "ov_prod_natveg", select = lvl)
    src <- "Timber|Volumetric|Production|Roundwood|Source|+|"
    bySource <- bind(toVol(readGDX(gdx, "ov_prod_forestry", select = lvl), paste0(src, "Planted forest")),
                     toVol(natveg[, , "secdforest"], paste0(src, "Secondary forest")),
                     toVol(natveg[, , "primforest"], paste0(src, "Primary forest")),
                     toVol(natveg[, , "other"], paste0(src, "Other land")),
                     toVol(readGDX(gdx, "ov73_prod_nonforest", select = lvl, react = "silent"), paste0(src, "Outside forest stocks")),
                     toVol(readGDX(gdx, "ov73_prod_residues", select = lvl, react = "silent"), paste0(src, "Harvest residues")))
    thinning <- bind(toVol(readGDX(gdx, "ov35_prod_thin_secdforest", select = lvl, react = "silent"), "Timber|Volumetric|Production|Roundwood|Thinning|+|Secondary forest"),
                     toVol(readGDX(gdx, "ov32_prod_thin_forestry", select = lvl, react = "silent"), "Timber|Volumetric|Production|Roundwood|Thinning|+|Other planted forest"))
    # felled stem of selectively logged stands, part of the secondary-forest source (gdx files with selective logging)
    logging <- NULL
    logArea <- readGDX(gdx, "ov35_log_secdforest", select = lvl, react = "silent")
    logStem <- readGDX(gdx, "p35_log_stem", react = "silent")
    if (!is.null(logArea) && !is.null(logStem)) {
      periods <- timePeriods(gdx)
      periods[, 1, ] <- 5 # forestry time step of the first year in MAgPIE
      logging <- toVol(logArea * logStem[, getYears(logArea), ] / periods,
                       "Timber|Volumetric|Production|Roundwood|Selective logging")
    }
    sawlog <- bind(toVol(readGDX(gdx, "ov_prod_sawlog_natveg", select = lvl, react = "silent"), "Timber|Volumetric|Production|Roundwood|Sawlog-grade|+|Natural vegetation"),
                   toVol(readGDX(gdx, "ov_prod_sawlog_forestry", select = lvl, react = "silent"), "Timber|Volumetric|Production|Roundwood|Sawlog-grade|+|Planted forest"))

    netTrade <- collapseNames(a[, , "Net-Trade"])
    getNames(netTrade) <- paste0("Timber|Volumetric|Net-Trade|Roundwood|+|", getNames(netTrade))
    netTrade <- mbind(setNames(dimSums(netTrade, dim = 3), "Timber|Volumetric|Net-Trade|+|Roundwood"),
                      netTrade)
    getNames(netTrade) <- paste0(getNames(netTrade), " (Mm3/yr)")

    exports <- collapseNames(a[, , "Exports"])
    getNames(exports) <- paste0("Timber|Volumetric|Exports|Roundwood|+|", getNames(exports))
    exports <- mbind(setNames(dimSums(exports, dim = 3), "Timber|Volumetric|Exports|+|Roundwood"),
                     exports)
    getNames(exports) <- paste0(getNames(exports), " (Mm3/yr)")

    imports <- collapseNames(a[, , "Imports"])
    getNames(imports) <- paste0("Timber|Volumetric|Imports|Roundwood|+|", getNames(imports))
    imports <- mbind(setNames(dimSums(imports, dim = 3), "Timber|Volumetric|Imports|+|Roundwood"),
                     imports)
    getNames(imports) <- paste0(getNames(imports), " (Mm3/yr)")

    out <- bind(dem, prod, bySource, thinning, logging, sawlog, netTrade, exports, imports)
  } else {
    message("Disabled (no timber) ", appendLF = FALSE)
  }

  return(out)
}
