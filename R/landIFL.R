#' @title landIFL
#' @description Land inside intact forest landscapes (IFL) by land type. The static IFL map
#'   (`f22_consv_prio`, category IFL) is capped by the land of each time step: IFL forest is
#'   assigned to primary forest first, the remainder to secondary forest; IFL other land is
#'   capped by other land. IFL on cropland and pasture (< 2 Mha) is treated as managed.
#'   Used for the managed/IFL split of the LPJmL land carbon sink in \code{reportEmissions}.
#' @export
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "cell", "reg", "glo" or "regglo"
#' @return IFL area (million ha) for primforest, secdforest and other, or NULL if the gdx
#'   has no `f22_consv_prio`
#' @author Florian Humpenoeder
#' @importFrom magclass getNames setNames setYears dimSums collapseNames mbind
#' @examples
#' \dontrun{
#' x <- landIFL(gdx)
#' }
landIFL <- function(gdx, file = NULL, level = "cell") {

  consv <- readGDX(gdx, "f22_consv_prio", react = "silent")
  if (is.null(consv) || !"IFL" %in% getNames(consv, dim = 1)) {
    return(NULL)
  }
  ifl <- setYears(collapseNames(consv[, , "IFL"]), NULL)
  a   <- land(gdx, level = "cell")

  # elementwise min of a static map (x) and a time-dependent cap
  .cap <- function(x, cap) {
    x <- cap * 0 + x
    x[x > cap] <- cap[x > cap]
    return(x)
  }

  iflForest <- dimSums(ifl[, , c("primforest", "secdforest")], dim = 3)
  prim  <- .cap(iflForest, collapseNames(a[, , "primforest"]))
  secd  <- .cap(iflForest - prim, collapseNames(a[, , "secdforest"]))
  other <- .cap(collapseNames(ifl[, , "other"]), collapseNames(a[, , "other"]))

  x <- mbind(setNames(prim, "primforest"),
             setNames(secd, "secdforest"),
             setNames(other, "other"))
  getSets(x)[3] <- "land"

  if (level != "cell") {
    x <- superAggregateX(x, level = level, aggr_type = "sum")
  }

  out(x, file)
}
