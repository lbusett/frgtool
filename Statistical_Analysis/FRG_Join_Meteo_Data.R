#'@title FRG_Join_Meteo_Data
#'@description Function used to retrieve the climatic data derived from WorldClim raster maps on the identified burnt areas
#'            and join them in a single data frame in which burnt areas are identified by OBJECTID and "described" in terms of their 
#'            average values of BioClim indicators + the Global Aridity Index
#' @returnType 
#'
#' @author Lorenzo Busetto (2012)
#' email: lorenzo.busetto@@jrc.ec.europa.eu
#'
#' Created Date: Nov 8, 2012
#' @export

# Folder where the climatic data are stored 
in_csv_folder = "E:/Projects_Data/Fire_Regeneration_Data/Climate_Data/Zonal_Statistics_Tables/CSV/"
in_files = list.files(in_csv_folder,pattern='.csv',full.names = T)

# Cycle on CSV files

for (file in in_files) {
  
  if (file == in_files [1]) {
    Meteo_Data = read.csv(file)
  } else {
    
    data_tmp =  read.csv(file)
    Meteo_Data = join(Meteo_Data, data_tmp)
  }
    
}

Meteo_data_File = "E:/Projects_Data/Fire_Regeneration_Data/Climate_Data/Meteo_Data.RData"
save(Meteo_Data, file = Meteo_data_File)