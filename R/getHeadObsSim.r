#' Extract simulated and observed head values from MODFLOW files 
#' 
#' This function combines head metadata from the .hob file and head data from 
#' the ._os file, returning a cleaned dataset of obname, wellID, x and y
#' coordinates, time, and simulated and observed head values.
#' @param hobfile Filepath to .hob file 
#' @param osfile Filepath to ._os file 
#' @param sort TRUE/FALSE. default =  TRUE. If true, the returned data.frame is 
#' sorted by time, then by well ID.
#' @keywords head simulated observed
#' @export
#' @examples
#' # specify model files
#' fname_hob <- 'BJ_3L_93_AnnualObs.hob'
#' fname_os <- 'BJ_3L_93_AnnualObs._os'
#' 
#' # run function
#' simObs <- getHeadObsSim(fname_hob, fname_os, sort = T)

getHeadObsSim <- function(hobfile, osfile, sort = TRUE) {
  # metadata -------------------------------------------------------------------
  # get rows to read in
  col1 <- read.csv(hobfile, header = F, as.is = TRUE) 
  rowend <- sum(grepl('#',col1[,1])) - 1
  
  # set column names and classes, and read in data
  colnames <- c('junk','junk1','junk2','ID','x.long','y.lat','time','obname')
  classes <- c(rep('character',6), 'factor','character')
  meta.raw <- read.table(hobfile, header=F, sep=' ', skip=1, comment.char='', 
                         col.names = colnames, colClasses = classes, 
                         nrows = rowend)
  # clean data (remove commas and nonsense columns, reformat)
  meta.raw$ID <- as.factor(gsub(",", "", meta.raw$ID, fixed = TRUE))
  meta.raw$x.long <- gsub(",", "", meta.raw$x.long, fixed = TRUE) 
  meta.raw2 <- meta.raw[,4:8]
  meta.raw2$x.long <- as.numeric(meta.raw2$x.long)
  meta.raw2$y.lat <- as.numeric(meta.raw2$y.lat)
  
  # head data ------------------------------------------------------------------
  data.raw <- read.csv(osfile,sep='')
  
  # combine with metadata
  data1 <- merge(meta.raw2, data.raw, by.x = 'obname', by.y ='OBSERVATION.NAME', 
                 all.y=F)
  
  # if sort = T, sort by time then by well
  if (sort == TRUE) data1 <- data1[order(data1$time, data1$ID),]  
  
  return(data1)
}

# # Jill's test
# directory <- 'S:/Users/deinesji/Beijing/Pilot/groundwater_model/1_Models_July2014_v10/BJ_3L_V10_AnnualObs/BJ_3L_93_AnnualObs_MODFLOW'
# fname_hob <- 'BJ_3L_93_AnnualObs.hob'
# fname_os <- 'BJ_3L_93_AnnualObs._os'
# osfile <- paste(directory,fname_os,sep='/')
# hobfile <- paste(directory,fname_hob,sep='/')
# 
# test <- getHeadObsSim(hobfile, osfile)
