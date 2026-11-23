#' @title GrowingStock
#' @description reads woody growing stock out of a MAgPIE gdx file
#'
#' @export
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "cell", "reg" (regional), "glo" (global), "regglo" (regional and global) or any secdforest aggregation level defined in superAggregate
#' @param indicator If the reported numbers are relative (mio m3/ha) or absolute (mio. m3). Default is relative.
#' @details Growing stock for producing woody materials consist of growing stock from plantations (forestry), secondary and primary forest as well as other land (natveg)
#' @return Growing stock in m3 per ha
#' @author Abhijeet Mishra
#' @importFrom magclass clean_magpie dimSums collapseNames setYears write.magpie
#' @examples
#'
#' \dontrun{
#' x <- GrowingStock(gdx)
#' }
#'
GrowingStock <- function(gdx, file = NULL, level = "regglo", indicator = "relative") {
  if (level %in% c("reg", "glo", "regglo") || isCustomAggregation(level)) {
    ac_sub <- readGDX(gdx, "ac")

    wood_density <- readGDX(gdx, "im_vol_conv", "f73_volumetric_conversion", react = "silent", format = "first_found")

    ## Read timber yield and convert from tDM/ha to m3/ha
    im_growing_stock <- readGDX(gdx, "im_growing_stock", "pm_timber_yield", react = "silent", format = "first_found") / wood_density

    ## Land information - cluster level
    land_forestry          <- readGDX(gdx, "ov32_land", "ov_land_fore", select = list(type = "level"), react = "silent")
    land_plantations       <- collapseNames(land_forestry[, , "plant"][, , ac_sub])
    # some gdx files carry the harvestable other planted forest pool with its own growing stock
    otherPlantedPool       <- intersect("other_planted", getNames(land_forestry, dim = 1))
    land_other_planted     <- if (length(otherPlantedPool)) collapseNames(land_forestry[, , otherPlantedPool][, , ac_sub]) else NULL
    gs_other_planted       <- readGDX(gdx, "im_growing_stock_oplant", react = "silent")
    if (is.null(gs_other_planted)) gs_other_planted <- im_growing_stock[, , "secdforest"] else gs_other_planted <- gs_other_planted / wood_density
    # afforestation: CO2-price AR ("aff") and NPI/NDC AR (every other type besides plantations and other planted forest)
    affTypes               <- setdiff(getNames(land_forestry, dim = 1), c("plant", otherPlantedPool))
    land_aff               <- collapseNames(dimSums(land_forestry[, , intersect("aff", affTypes)][, , ac_sub], dim = "type32"))
    land_ndc               <- collapseNames(dimSums(land_forestry[, , setdiff(affTypes, "aff")][, , ac_sub], dim = "type32"))
    land_afforest          <- land_aff + land_ndc
    land_secdforest        <- collapseNames(readGDX(gdx, "ov35_secdforest", select = list(type = "level"))[, , ac_sub])
    land_primforest        <- setNames(collapseNames(readGDX(gdx, "ov_land", select = list(type = "level"))[, , "primforest"]), "acx")
    land_other             <- collapseNames(readGDX(gdx, "ov_land_other", "ov35_other", select = list(type = "level"),
                                                    format = "first_found")[, , ac_sub])
    if (getSets(land_other, fulldim = FALSE)[[3]] == "othertype35.ac") land_other <- dimSums(land_other, dim = "othertype35")
    land_natfor            <- land_secdforest
    land_natfor[, , "acx"] <- land_natfor[, , "acx"] + land_primforest
    land_forest_total      <- land_plantations + land_afforest + land_natfor
    if (!is.null(land_other_planted)) land_forest_total <- land_forest_total + land_other_planted

    ## Only use ac_sub
    land_plantations       <- land_plantations[, , ac_sub]
    land_aff               <- land_aff[, , ac_sub]
    land_ndc               <- land_ndc[, , ac_sub]
    if (!is.null(land_other_planted)) {
      land_other_planted   <- land_other_planted[, , ac_sub]
      gs_other_planted     <- collapseNames(gs_other_planted[, , ac_sub])
    }
    land_secdforest        <- land_secdforest[, , ac_sub]
    land_natfor            <- land_natfor[, , ac_sub]
    land_other             <- land_other[, , ac_sub]
    land_forest_total      <- land_forest_total[, , ac_sub]
    im_growing_stock        <- im_growing_stock[, , ac_sub]
    # stand-density state of secondary forest (gdx with p35_dens_post): stock beyond the curve
    gs_secdforest <- collapseNames(im_growing_stock[, , "secdforest"])
    densPost <- readGDX(gdx, "p35_dens_post", react = "silent")
    if (!is.null(densPost)) gs_secdforest <- gs_secdforest * densPost[, getYears(gs_secdforest), getItems(gs_secdforest, dim = 3)]

    ## Yields x land = standing growing stocks (mio. m3)

    growing_stock_main <- superAggregateX(
      data = mbind(
        setNames(dimSums(land_plantations * im_growing_stock[, , "forestry"],              dim = 3), "plantations"),
        setNames(dimSums(land_aff         * im_growing_stock[, , "secdforest"],            dim = 3), "aff"),
        setNames(dimSums(land_ndc         * im_growing_stock[, , "secdforest"],            dim = 3), "ndc"),
        setNames(dimSums(land_secdforest  * gs_secdforest,                                 dim = 3), "secdforest"),
        setNames(dimSums(land_primforest  * im_growing_stock[, , "primforest"][, , "acx"], dim = 3), "primforest"),
        setNames(dimSums(land_other       * im_growing_stock[, , "other"],                 dim = 3), "other")
      ),
      aggr_type = "sum", level = level
    )
    if (!is.null(land_other_planted)) {
      gsOtherPlanted <- setNames(dimSums(land_other_planted * gs_other_planted, dim = 3), "other_planted")
      growing_stock_main <- mbind(growing_stock_main, superAggregateX(data = gsOtherPlanted, aggr_type = "sum", level = level))
    }

    ## Create GS for natural and total forests, as this is absolute amount still it can be added up
    growing_stock_natfor <- setNames(growing_stock_main[, , "secdforest"] + growing_stock_main[, , "primforest"], "natfor")

    growing_stock_forest_total <- setNames(growing_stock_natfor + growing_stock_main[, , "plantations"] +
                                             growing_stock_main[, , "aff"] + growing_stock_main[, , "ndc"], "forest")
    if (!is.null(land_other_planted)) {
      growing_stock_forest_total <- setNames(growing_stock_forest_total + growing_stock_main[, , "other_planted"], "forest")
    }
    # FRA forest holds no carbon-price afforestation: the total without it is the one to compare with FRA
    growing_stock_forest_excl_aff <- setNames(growing_stock_forest_total - growing_stock_main[, , "aff"], "forest_excl_aff")

    a <- mbind(growing_stock_forest_total, growing_stock_forest_excl_aff, growing_stock_natfor, growing_stock_main)

    ## In case relative numbers are needed, divide by aggregated land information

    if (indicator == "relative") {
      # stock per hectare; a pool with (numerically) no area reports 0
      rel <- function(gs, land) {
        area <- superAggregateX(data = land, aggr_type = "sum", level = level)
        gs <- gs / area
        gs[area < 1e-6] <- 0
        gs
      }
      growing_stock_forest_total  <- rel(growing_stock_forest_total, dimSums(land_forest_total, dim = "ac"))
      growing_stock_forest_excl_aff <- rel(growing_stock_forest_excl_aff, dimSums(land_forest_total - land_aff, dim = "ac"))
      growing_stock_natfor        <- rel(growing_stock_natfor, dimSums(land_natfor, dim = "ac"))
      growing_stock_plantations   <- rel(growing_stock_main[, , "plantations"], dimSums(land_plantations, dim = "ac"))
      growing_stock_aff           <- rel(growing_stock_main[, , "aff"], dimSums(land_aff, dim = "ac"))
      growing_stock_ndc           <- rel(growing_stock_main[, , "ndc"], dimSums(land_ndc, dim = "ac"))
      growing_stock_primforest    <- rel(growing_stock_main[, , "primforest"], dimSums(land_primforest, dim = 3))
      growing_stock_secdforest    <- rel(growing_stock_main[, , "secdforest"], dimSums(land_secdforest, dim = "ac"))
      growing_stock_other         <- rel(growing_stock_main[, , "other"], dimSums(land_other, dim = "ac"))
      if (!is.null(land_other_planted)) {
        growing_stock_other_planted <- rel(growing_stock_main[, , "other_planted"], dimSums(land_other_planted, dim = "ac"))
        growing_stock_other         <- mbind(growing_stock_other, growing_stock_other_planted)
      }
      growing_stock_main          <- mbind(growing_stock_plantations, growing_stock_aff, growing_stock_ndc, growing_stock_secdforest,
                                           growing_stock_primforest, growing_stock_other)
      a <- mbind(growing_stock_forest_total, growing_stock_forest_excl_aff, growing_stock_natfor, growing_stock_main)
    }
  } else {
    message("ERROR - aggregation level not supported")
    a <- NULL
  }

  out(a, file)
}
