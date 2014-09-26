#' Create binary raster grid of class RasterLayer based on polygon boundary. 
#' 
#' This function takes the extent and CRS from boundary polygon, and creates a
#' raster object with specified cell size where active cells have value = 1 and 
#' inactive cells outside boundary have value = 0. Cell size can be specified by 
#' resolution or nrows and ncols. The extent can be specified if differnt
#' from the polygon input, which is used to assign active cells.
#' @param polybound Polygon of class 'SpatialPolygonsDataFrame' 
#' @param res Resolution for raster cells. Numeric vector of length 1 or 2. 
#' @param nrows Number of rows. If specified, overrides `res` argument
#' @param ncols Number of columns. If specified, overrides `res` argument
#' @param ext Alternative to using polybound, specifies extent. Matrix bounding
#' box format: matrix(c(xmin,ymin,xmax,ymax),2,2)   
#' @param vis TRUE/FALSE. Do you want a plot made of the outcome? default =  FALSE
#' @param outFile Optional. Filename for raster, including extension; if entered, 
#' will write to disk
#' @keywords binary raster
#' @export
#' @examples
#' # specify resolution  ------------------
#' poly <- readOGR('S:/Users/deinesji/HPA/gis/lhm', 'RRB_bound2_6500m', verbose=F)
#' cellSize = 1000
#' outFileName = 'S:/Users/deinesji/HPA/gis/lhm/rrb1_surfaceGrid_1000m.img'
#' 
#' # create raster object in R
#' test <- createBinaryGrid(polybound = poly, resolution = cellSize)
#' # manually view
#' plot(test)
#' plot(poly, add=T)
#' 
#' create raster object in R, plot result
#' test <- createBinaryGrid(polybound = poly, resolution = cellSize, vis = T)
#' 
#' # create raster object in R, plot result, write out Raster
#' test <- createBinaryGrid(polybound = poly, resolution = cellSize, vis = T,
#'                         outFile = outFileName)
#'                         
#' # specify nrow/ncell  ---------------
#' poly <- readOGR('S:/Users/deinesji/HPA/gis/lhm', 'RRB_bound2_6500m', verbose=F)
#' nr = 286
#' nc = 496
#' outFileName = 'S:/Users/deinesji/HPA/gis/lhm/rrb1_surfaceGrid_1000m_test.img'
#' 
#' # create raster object in R
#' test2 <- createBinaryGrid(polybound = poly, ncols = nc, nrows = nr)
#' 
#' # specify extent ------------------
#' poly <- readOGR('S:/Users/deinesji/HPA/gis/lhm', 'RRB_bound2_6500m', verbose=F)
#' cellSize = 1000
#' outFileName = 'S:/Users/deinesji/HPA/gis/lhm/rrb1_surfaceGrid_1000m.img'
#' # create extent matrix
#' bbox <- matrix(c(-245000,-153000,273000,154000),2,2)
#' # create raster object in R
#' test3 <- createBinaryGrid(polybound = poly, resolution = cellSize, ext = bbox)                         

 
createBinaryGrid <- function(polybound, resolution, nrows=NA, ncols=NA, 
                             ext=NA, vis = F, outFile = '') {
  # if not specified, set extent to polygon boundary
  if (length(ext) == 1) ext <- bbox(polybound)
  # get CRS from polygon boundary
  crs1 <- proj4string(polybound)             
  
  # create raster with default value: 1
  if (is.na(nrows) & is.na(ncols)) {   # if resolution is specified                
    r <- raster(nrow = 100, ncol = 100, extent(ext))
    res(r) <- resolution
    proj4string(r) <- crs1
    r[] <- 1
  } else if (!is.na(nrows) & !is.na(ncols)) {  # if rows, columns specified
    r <- raster(nrow = nrows, ncol = ncols, extent(ext))
    proj4string(r) <- crs1
    r[] <- 1          
  } else { 
    stop('If specifying nrows/ncols, both must be specified')
  }
  
  # update values outside of polygon to 0
  rMask <- mask(r, polybound, updatevalue = 0)
  
  # if specified, make plot
  if (vis != F) {
    plot(rMask)
    plot(polybound, add=T)
  }
  
  # if specified, write out raster to disk
  if (nchar(outFile) > 0) {
    writeRaster(rMask, outFile, dataType = 'INT1U')
  }
  
  return(rMask)
}



# # resolution specified tests ------------------
# poly <- readOGR('S:/Users/deinesji/HPA/gis/lhm', 'RRB_bound2_6500m', verbose=F)
# cellSize = 1000
# outFileName = 'S:/Users/deinesji/HPA/gis/lhm/rrb1_surfaceGrid_1000m.img'
# 
# # create raster object in R
# test <- createBinaryGrid(polybound = poly, resolution = cellSize)
# 
# # create raster object in R, plot result
# test <- createBinaryGrid(polybound = poly, resolution = cellSize, vis = T)
# 
# # create raster object in R, plot result, write out Raster
# test <- createBinaryGrid(polybound = poly, resolution = cellSize, vis = T,
#                          outFile = outFileName)

# # nrow/ncell specified tests ---------------
# poly <- readOGR('S:/Users/deinesji/HPA/gis/lhm', 'RRB_bound2_6500m', verbose=F)
# nr = 286
# nc = 496
# outFileName = 'S:/Users/deinesji/HPA/gis/lhm/rrb1_surfaceGrid_1000m_test.img'
# 
# # create raster object in R
# test2 <- createBinaryGrid(polybound = poly, ncols = nc, nrows = nr)
# 
# # create raster object in R, plot result
# test2 <- createBinaryGrid(polybound = poly, ncols = nc, nrows = nr, vis = T)
# 
# # create raster object in R, plot result, write out Raster
# test2 <- createBinaryGrid(polybound = poly, ncols = nc, nrows = nr, vis = T,
#                          outFile = outFileName)


# different extent tests ------------------
poly <- readOGR('S:/Users/deinesji/HPA/gis/lhm', 'RRB_bound2_6500m', verbose=F)
cellSize = 1000
outFileName = 'S:/Users/deinesji/HPA/gis/lhm/rrb1_surfaceGrid_1000m.img'
bbox <- matrix(c(-245000,-153000,273000,154000),2,2)

# create raster object in R
test3 <- createBinaryGrid(polybound = poly, resolution = cellSize, ext = bbox)

# create raster object in R, plot result
test <- createBinaryGrid(polybound = poly, resolution = cellSize, ext = bbox, vis = T)

# create raster object in R, plot result, write out Raster
test <- createBinaryGrid(polybound = poly, resolution = cellSize, vis = T,
                         outFile = outFileName, ext = bbox)
