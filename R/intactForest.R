#' @title intactForest
#' @description Intact forest area by forest type. The static intact forest map of Grassi et al. (2023), input
#'   file forest_grassi2023_<ctype>.mz, is assigned to primary forest first and to secondary forest after,
#'   capped by the forest area of each time step. Non-intact forest is all other forest, planted forest included.
#' @export
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "cell", "reg", "glo" or "regglo"
#' @param dir folder holding forest_grassi2023_<ctype>.mz; by default the folder of the gdx
#' @return Intact forest area (million ha) for primforest and secdforest, or NULL if the input file is missing
#' @author Florian Humpenoeder
#' @importFrom magclass read.magpie getCells getSets<- collapseNames mbind setNames
#' @examples
#' \dontrun{
#' x <- intactForest(gdx)
#' }
intactForest <- function(gdx, file = NULL, level = "cell", dir = dirname(gdx)) {

  f <- Sys.glob(file.path(dir, "forest_grassi2023_c*.mz"))
  if (length(f) == 0) {
    return(NULL)
  }
  if (length(f) > 1) stop("More than one forest_grassi2023 file in ", dir)

  a   <- land(gdx, level = "cell")
  map <- read.magpie(f)
  if (!setequal(getCells(map), getCells(a))) stop("Simulation units of ", basename(f), " do not match the gdx")

  .cap <- function(x, cap) {
    x[x > cap] <- cap[x > cap]
    return(x)
  }
  intact <- collapseNames(a[, , "primforest"]) * 0 + collapseNames(map[getCells(a), , "intact"])
  prim <- .cap(intact, collapseNames(a[, , "primforest"]))
  secd <- .cap(intact - prim, collapseNames(a[, , "secdforest"]))

  x <- mbind(setNames(prim, "primforest"), setNames(secd, "secdforest"))
  getSets(x)["d3.1"] <- "land"

  if (level != "cell") {
    x <- superAggregateX(x, level = level, aggr_type = "sum")
  }

  out(x, file)
}
