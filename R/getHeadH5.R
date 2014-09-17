#' Get head from GMS output
#' 
#' This function translates head values exported as .h5 from GMS into rasters
#' @param h5file Filepath to exported .h5 file
#' @param rTemplate Model grid raster of Formal class RasterLayer
#' @param layer Model layer to extract head. Only layer = 1 works.
#' @param startYear Starting year of your model, used to name raster grids
#' @keywords head
#' @export
#' @examples
#' getHeadH5()

getHeadH5 <- function(h5file, rTemplate, layer, startYear) {
    if (layer ==1) {
      # load head data and get dimensions
      headData <- h5read(h5file, 'Datasets/Head/Values')
      dimYears <- dim(headData)[2]
    
      # convert to model grid for each year
      rows <- nrow(rTemplate)       # get number of rows
      cells <- ncell(rTemplate)     # get number of cells
      head <- list()                # create list to populate
    
      for (i in 1:dimYears) { 
        annual <- matrix(headData[1:cells,i],rows,byrow=T)  
        head[[i]] <- raster(annual, template=rTemplate)
      }
      
      # post-process
      names(head) <- startYear:(startYear + (dimYears-1))  # name raster Years
      headStack <- stack(head)                             # stack rasters
      
    } else {
      stop('Function can currently only process layer 1 head')
    }
    
    return(headStack)
}

# ---------- testing

# source("http://bioconductor.org/biocLite.R")
# biocLite("rhdf5")
# 
# library(rhdf5)
# library(raster)
# 
# # create template raster grid (165 rows, 326 columns)    
# r <- raster(ncol = 127, nrow = 114)  # create raster
# boundbox <- matrix(c(394328, 4372600, 521328, 4486600), 2, 2)  # c(xmin,ymin,xmax,ymax),r,c
# extent(r) <- extent(boundbox)  # give raster extent 
# proj4string(r) = "+proj=utm +zone=50 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
# rTemplate <- r
#     		 
# directory2 <- 'S:/Users/deinesji/Beijing/Pilot/groundwater_model/1_Models_1993_start_March2014/jillExported'
# headh5 <- 'Head.h5'
# h5file <- paste(directory2, headh5, sep='/')

# --- Run function

#testStack <- getHeadH5(h5file, rTemplate, layer = 1, startYear = 1993)
#spplot(testStack)

#testStack <- getHeadH5(h5file, rTemplate, layer = 2, startYear = 1993)




