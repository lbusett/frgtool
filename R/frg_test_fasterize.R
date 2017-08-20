library(tidyverse)
library(sf)
library(raster)
library(fasterize)

in_burned_file   <- "D:/Temp/tempfrg/Input_Shapefiles/Burned_Areas_00_15.shp"
in_rast_template <- raster(raster("D:/Temp/tempfrg/Scaled_Indexes/Med_SNDVI/Yearly_Images/2001/Med_SNDVI_2001")) 
#   ____________________________________________________________________________
#   Prepare the shapefile - remove invalid geoms and cast all to multipolygo####
frg_create_shapefiles <- function(in_burned_file, in_rast_template) {
  
  in_burned  <- in_burned_file %>% 
    st_read(stringsAsFactors = FALSE) 
  st_crs(in_burned) <- 3035 
  
  #   ____________________________________________________________________________
  #   rasterize the burned area shape and save to tif, then write it to tiff  ####
  
  rasterized_shape <- fasterize(in_burned, in_rast_template, fun = "sum")
  
  rasterized_shape_tif <- tempfile(fileext = ".tif")
  repolygonized_shape  <- tempfile(fileext = ".shp") 
  
  tr <- blockSize(rasterized_shape)
  s  <- raster(rasterized_shape)
  s  <- writeStart(rasterized_shape,
                  filename  = rasterized_shape_tif,
                  overwrite = TRUE,
                  datatype  = "INT1U",
                  options = "COMPRESS=DEFLATE")
  
  for (i in 1:tr$n) {
    v <- getValuesBlock(rasterized_shape, row = tr$row[i], nrows = tr$nrows[i])
    s <- writeValues(s, v, tr$row[i])
  }
  s <- writeStop(s)
  
  #   ____________________________________________________________________________
  #   Re-polygonyze the rasterized shape to have one polygon for each         ####
  #   intersection, with value equal to the number of overlaps (i.e., number
  #   of times the area burned in the analyzed period)
  
  final_polys <- gdal_polygonizeR(rasterized_shape_tif,
                                  repolygonized_shape, 
                                  quiet = FALSE,
                                  overwrite = T)

  #   ____________________________________________________________________________
  #   Process areas burned onces                                                  ####
  
  burned_once <- final_polys %>% 
    filter(n_inters == 1)
  
  # Validate geometries
  valid = st_is_valid(in_burned)
  if (max(valid) != 0) {
    in_burned[which(valid == FALSE),] = st_cast(st_buffer(in_burned[which(valid == FALSE),], 0.01), "MULTIPOLYGON")
  }
  
  valid = st_is_valid(burned_once)
  if (max(valid) != 0) {
    burned_once[which(valid == FALSE),] = st_cast(st_buffer(burned_once[which(valid == FALSE),], 0.01), "MULTIPOLYGON")
  }
  st_crs(burned_once) <- 3035 
  # Re-intersect with original shape to retrieve attributes
  intersected <- sf::st_intersection(burned_once, in_burned)
  tmp <- intersected
  
  a = intersected %>% 
    dplyr::group_by(ID) %>%
      summarize_at(c(1:22), first) 
  
  a$area <- st_area(a)
  a <- filter(a, as.numeric(area) >= 1000)
  
}






# 
# 
# 
# for (i in seq_along(in_orig$id)) in_orig[i,] <- st_cast(in_orig[i,], "MULTIPOLYGON")
# valid = st_is_valid(in_orig)
# if (max(valid) != 0) {
#   in_orig[which(valid == FALSE),] = st_cast(st_buffer(in_orig[which(valid == FALSE),], 0.01), "MULTIPOLYGON")
# }
# in_orig <- st_cast(in_orig, "MULTIPOLYGON")
# 
# start.time <- Sys.time()
# 
# 
# lgt = NA
# out_single = list()
# out_multi = list()
# count_single <- 1
# count_multi  <- 1
# overlap_ID   <- 0
# FID_Burned   <- 0
# out_lut      <- NULL
# # for (id_ind in seq_along(along.with = in_orig$ID)) {
# for (id_ind in 1:100) {
#   print(id_ind)
#   id = in_orig$ID[id_ind]
#   
#   
#   single_geom <- in_orig[which(in_orig$ID == id), ]
#   all_others  <- in_orig[which(in_orig$ID != id), ]
#   inters_codes <- st_intersects(single_geom, all_others, sparse = FALSE)
#   n_inters    <- length(which(inters_codes ==TRUE))
#   
#   if (n_inters > 0) {
#     
#     intersections <- st_intersection(single_geom, all_others)
#     intersections <- intersections[which(st_dimension(intersections) == 2),]
#     inters_codes  <- inters_codes[which(st_dimension(intersections) == 2)]   
#     n_inters      <- length(inters_codes)
#     if (length(intersections$ID) != 0 ) {
#       
#       
#       if (any(st_is(intersections, "GEOMETRYCOLLECTION"))) {
#         which_mixed        <- which(st_is(intersections, "GEOMETRYCOLLECTION") == TRUE)
#         newintersections   <- intersections[which(st_is(intersections, "GEOMETRYCOLLECTION") == FALSE ), ] 
#         int_split          <- intersections[which(st_is(intersections, "GEOMETRYCOLLECTION") == TRUE ),  ] 
#         if (any(st_is(int_split ,"POLYGON") | st_is(int_split ,"MULTIPOLYGON"))) {
#           int_split        <- int_split[which(st_is(int_split ,"POLYGON") | st_is(int_split ,"MULTIPOLYGON")), ]
#           attr(int_split, "sf_column") = "geoms"
#           newintersections <- rbind(newintersections, int_split)
#         } else {
#           intersections    <- newintersections
#         }
#       }
#       
#       
#       intersections <- intersections[which(st_dimension(intersections) == 2),]
#       # inters_codes  <- inters_codes[which(st_dimension(intersections) == 2)]
#       
#       
#       if (length(intersections$ID) != 0 ) {
#         #   ____________________________________________________________________________
#         #   Identify intersecting geoms                                             ####
#         area_ints <- st_area(intersections)/10000
#         for (i in seq_along(intersections$ID)) {
#           # browser()
#           int_single <- intersections[i,]
#           n_int      <- (length(names(int_single)) + 1) / length(names(single_geom))
#           int_ids    <- as.numeric(as_data_frame(int_single[1, c(1, length(names(single_geom))*(1:(n_int - 1)))])[1,1:n_int])
#           # intersections[i,] <- st_cast(int_single, "MULTIPOLYGON")
#           out_multi[[overlap_ID + 1]] <- int_single  %>% 
#             mutate(FID_Burned = FID_Burned, 
#                    overlap_ID = overlap_ID) %>% 
#             dplyr::mutate(Area_Int = area_ints[i]) %>% 
#             dplyr::select(FID_Burned, Area_Int, overlap_ID) %>% 
#             st_cast("MULTIPOLYGON") %>% 
#             dplyr::mutate(ints_codes = paste(int_ids, collapse = " "))
#           
#           # add lines to csv LUT data frame
#           
#           fire_dates  <- as_data_frame(int_single[1, c(2, 1 + length(names(single_geom))*(1:(n_int - 1)))])[1,1:n_int]
#           placenames  <- as.character(as_data_frame(int_single[1, c(5, 4 + length(names(single_geom))*(1:(n_int - 1)))])[1,1:n_int])
#           yearseasons <- as.character(as_data_frame(int_single[1, c(11, 10 + length(names(single_geom))*(1:(n_int - 1)))])[1,1:n_int])
#           out_lut     <- rbind(out_lut, data.frame(OBJECTID   = int_ids,
#                                                    FireDate   = format(as.Date(as.character(fire_dates[1,])),  "%d-%m-%Y"), 	
#                                                    Place_Name = placenames,
#                                                    YearSeason = yearseasons,
#                                                    OVERLAP_ID = overlap_ID))
#           overlap_ID <- overlap_ID + 1
#           
#         }
#         # browser()
#         difference    <- sf::st_difference(single_geom, sf::st_union(intersections))
#         
#         if (length(difference$ID != 0)) {
#           out_single[[count_single]] <- st_cast(difference, "MULTIPOLYGON")
#           # out_multi[[count_multi]]   <- st_cast(intersections, "MULTIPOLYGON")
#           count_single               <- count_single + 1
#           count_multi                <- count_multi + 1
#           FID_Burned <- FID_Burned + 1 
#         }
#         
#       } else {
#         out_single[[count_single]] <- single_geom
#         count_single               <- count_single + 1
#         
#       }
#       # difference
#     } else {
#       out_single[[count_single]] <- single_geom
#       count_single               <- count_single + 1
#     }
#   } else {
#     out_single[[count_single]] <- single_geom
#     count_single               <- count_single + 1
#     # single_geom
#   }
# }  
# 
# end.time <- Sys.time()
# start.time - end.time
# 
# 
# out_single_all <- out_single %>% 
#   data.table::rbindlist() %>% 
#   as_tibble() %>% 
#   arrange(ID) %>% 
#   st_as_sf() 
# 
# out_multi_all <- out_multi %>% 
#   data.table::rbindlist() %>% 
#   as_tibble() %>% 
#   arrange(ID) %>% 
#   st_as_sf() 





# %>% 
#   as("sf")
# # intersect   <- st_intersection(single_geom, crop_union)
# diff        <- st_difference(crop_union, single_geom)
# # all_others      <- st_difference(union_orig, single_geom)
# # out_single_geom <- st_overlaps(single_geom, union_orig) 
# # lgt = rbind(lgt, length(out_single_geom[[1]]))
# print(st_bbox(diff))
# if (!is.na(st_bbox(diff)[1])) {
#   browser()
#   out_single[[id_ind]] <- st_difference(single_geom, union_orig)
#   out_multi[[id_ind]]  <- st_intersection(single_geom, union_orig)
# } else {
#   out_single[[id_ind]] <- single_geom
# }
# }
# end.time <- Sys.time()
# time.taken <- end.time - start.time
# time.taken
# 
# out_single <- in_orig
# st_geometry(out_single) <- out_single_geom
# 
# out_singlesp <- as(out_single, "Spatial")
# 
# out_multi <- st_union(in_orig) %>% 
#   st_difference(in_orig)
# 
# 
# 
# 
# 
# 
# in_single  <- "D:/Temp/tempfrg/Intermed_Proc/Shapefiles/Burned_Areas_00_15_Single_Fires.shp" %>% 
#   st_read(stringsAsFactors = FALSE)  %>% 
#   st_intersection(adm) %>% 
#   as("Spatial")
# 
# in_multi   <- "D:/Temp/tempfrg/Intermed_Proc/Shapefiles/Burned_Areas_00_15_Multiple_Fires.shp" %>% 
#   st_read() %>% 
#   st_intersection(adm) 
# 
# in_multi <- arrange(in_multi, FID_Burned)
# 
# 
# mapview(as(in_single, "Spatial"), zcol = "ID")+
#   mapview(as(in_multi, "Spatial"), zcol = "FID_Burned") +
#   mapview(as(in_orig, "Spatial"), zcol = "ID")
# 
# pro = st_difference(in_orig, st_union(in_orig))
# 
# val  = st_make_valid(in_orig)
# 
# names(in_orig)
# names(in_single)
# names(in_multi)
