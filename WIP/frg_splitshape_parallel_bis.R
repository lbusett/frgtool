library(tidyverse)
library(sf)
library(raster)
library(foreach)
library(tibble)
library(doParallel)
# 
# adm <- getData("GADM", country="ITA", level=1) %>% 
#   as("sf") %>% 
#   filter(NAME_1 =="Sardegna") %>% 
#   st_transform(3035)

# st_crs(adm) = st_crs(st_read("/home/lb/Google_Drive/FRG/Input_Shapefiles/Burned_Areas_00_15.shp"))

in_orig    <- "D:/Temp/tempfrg/Input_Shapefiles/Burned_Areas_00_15.shp" %>% 
  st_read(stringsAsFactors = FALSE) 
st_crs(in_orig) <- 3035 
# st_intersection(adm)
# tmp = in_orig
# 
# in_single    <- "/home/lb/Google_Drive/FRG/Intermediate_Processing/Shapefiles/Burned_Areas_00_15_Single_Fires.shp" %>% 
#   st_read(stringsAsFactors = FALSE)   %>% 
#   st_intersection(adm)
# 
# in_multi    <- "/home/lb/Google_Drive/FRG/Intermediate_Processing/Shapefiles/Burned_Areas_00_15_Multiple_Fires.shp" %>% 
#   st_read(stringsAsFactors = FALSE)   %>% 
#   st_intersection(adm) %>% 
#   as("Spatial")
# 

# tmp = in_orig

for (i in seq_along(in_orig$id)) in_orig[i,] <- st_cast(in_orig[i,], "MULTIPOLYGON")
valid = st_is_valid(in_orig)
if (max(valid) != 0) {
  in_orig[which(valid == FALSE),] = st_cast(st_buffer(in_orig[which(valid == FALSE),], 0.01), "MULTIPOLYGON")
}

in_orig <- st_cast(in_orig, "MULTIPOLYGON")
start.time <- Sys.time()
cl <- parallel::makeCluster(2)  
doParallel::registerDoParallel(cl)  
FID_Burned   <- 0
overlap_ID   <- 0
out_lut = NULL
n_IDs = length(in_orig$ID)
list_int_ids = NULL
# pb <- tcltk::tkProgressBar("Parallel task", min = 1, max = 100)
results <- foreach::foreach(id_ind = 1:100, .packages = c("tcltk", "sf", "dplyr", "tibble", "data.table" )) %dopar% {
  
  # for (id_ind in 1:100) {
  # if (!exists("pb2")) {
  #   pb2 <- tcltk::tkProgressBar("Parallel task", min = 1, max = 100)
  # }
  # tcltk::setTkProgressBar(pb2, id_ind)
  # Sys.sleep(0.05)
  
  print(id_ind)
  id = in_orig$ID[id_ind]
  single_geom <- in_orig[which(in_orig$ID == id), ]
  all_others  <- in_orig[which(in_orig$ID != id), ]
  inters_codes <- sf::st_intersects(single_geom, all_others, sparse = FALSE)
  n_inters    <- length(which(inters_codes == TRUE))
  
  if (n_inters > 0) {
    intersections <- sf::st_intersection(single_geom, all_others)
    difference    <- sf::st_difference(single_geom, sf::st_union(intersections))
    intersections <- intersections[which(st_dimension(intersections) == 2),]
    # inters_codes  <- inters_codes[which(st_dimension(intersections) == 2)]   
    # n_inters      <- length(inters_codes)
    if (length(intersections$ID) != 0 ) {
      if (any(st_is(intersections, "GEOMETRYCOLLECTION"))) {
        which_mixed        <- which(sf::st_is(intersections, "GEOMETRYCOLLECTION") == TRUE)
        newintersections   <- intersections[which(sf::st_is(intersections, "GEOMETRYCOLLECTION") == FALSE ), ] 
        int_split          <- intersections[which(sf::st_is(intersections, "GEOMETRYCOLLECTION") == TRUE ),  ] 
        if (any(sf::st_is(int_split ,"POLYGON") | sf::st_is(int_split ,"MULTIPOLYGON"))) {
          int_split        <- int_split[which(sf::st_is(int_split ,"POLYGON") | sf::st_is(int_split ,"MULTIPOLYGON")), ]
          attr(int_split, "sf_column") = "geoms"
          newintersections <- rbind(newintersections, int_split)
        } else {
          intersections    <- newintersections
        }
      }
      areas <- as.numeric(sf::st_area(intersections))/10000
      intersections <- intersections[which(sf::st_dimension(intersections) == 2),]
      intersections <- intersections[areas >= 0.5,]
    }
    if (length(intersections$ID) != 0 ) {
      # if (length(intersections$ID) != 0 ) {
      # # inters_codes  <- inters_codes[which(st_dimension(intersections) == 2)]
      # 
      # 
      # 
      #   #   ____________________________________________________________________________
      #   #   Identify intersecting geoms                                             ####
      #   area_ints <- as.numeric(sf::st_area(intersections)/10000)
      #   out_multis <- NULL
      #   for (i in seq_along(along = intersections$ID)) {
      #     # browser()
      #     int_single <- intersections[i,]
      #     n_int      <- (length(names(int_single)) + 1) / length(names(single_geom))
      #     int_ids    <- int_single[1, c(1, length(names(single_geom))*(1:(n_int - 1)))] %>% 
      #       tibble::as_data_frame() %>% 
      #       as.numeric() %>% 
      #       sort()
      #     if (length(intersect(int_ids, list_int_ids)) == 0) {
      #       # intersections[i,] <- st_cast(int_single, "MULTIPOLYGON")
      #       out_multi <- int_single  %>% 
      #         dplyr::mutate(FID_Burned = FID_Burned, overlap_ID = overlap_ID) %>% 
      #         dplyr::mutate(Area_Int = area_ints[i]) %>% 
      #         dplyr::select(FID_Burned, Area_Int, overlap_ID) %>% 
      #         sf::st_cast("MULTIPOLYGON") %>% 
      #         dplyr::mutate(int_ids = paste(int_ids, collapse = " "))
      #       
      #       if (i == 1) {
      #         out_multis <- out_multi
      #       } else {
      #         out_multis <- rbind(out_multis, out_multi)
      #       }
      #       # add lines to csv LUT data frame
      #       
      #       fire_dates  <- tibble::as_data_frame(int_single[1, c(2, 1 + length(names(single_geom))*(1:(n_int - 1)))])[1,1:n_int]
      #       placenames  <- as.character(tibble::as_data_frame(int_single[1, c(5, 4 + length(names(single_geom))*(1:(n_int - 1)))])[1,1:n_int])
      #       yearseasons <- as.character(tibble::as_data_frame(int_single[1, c(11, 10 + length(names(single_geom))*(1:(n_int - 1)))])[1,1:n_int])
      #       out_lut     <- rbind(out_lut, data.frame(OBJECTID   = int_ids,
      #                                                FireDate   = format(as.Date(as.character(fire_dates[1,])),  "%d-%m-%Y"), 	
      #                                                Place_Name = placenames,
      #                                                YearSeason = yearseasons,
      #                                                OVERLAP_ID = overlap_ID, 
      #                                                int_ids    = paste(int_ids, collapse = " ")))
      #       overlap_ID <- overlap_ID + 1
      #       list_int_ids <- rbind(list_int_ids, paste(int_ids, collapse = " "))  
      #     } 
      #   }
      #   
      # out_multis <- data.table::rbindlist(out_multis)
      # browser()
      
      
      # if (length(difference$ID != 0)) {
      # FID_Burned <- FID_Burned + 1 
      return(list(single  = sf::st_cast(difference, "MULTIPOLYGON"),
                  multi   = sf::st_cast(intersections, "MULTIPOLYGON")))
      # , 
      #      out_lut = out_lut))
    } else {
      return(list(single  = sf::st_cast(difference, "MULTIPOLYGON"),
                  multi   = NULL, 
                  out_lut = NULL))
      
    }
  } else {
    return(list(single  = sf::st_cast(single_geom, "MULTIPOLYGON"), 
                multi   = NULL, 
                out_lut = NULL))
  }
  # } else {
  #   return(list(single  = sf::st_cast(single_geom, "MULTIPOLYGON"), 
  #               multi   = NULL, 
  #               out_lut = NULL))
  # }
}  
parallel::stopCluster(cl)

out_single = list()
out_multi = list()
count_single <- 1
count_multi  <- 1
for (i in 1:100) {
  single <- results[[i]]$single
  multi <- results[[i]]$multi
  if (length(single$ID) != 0) {
    if (as.numeric(sf::st_area(single)/10000) >= 0.5) {
      out_single[[count_single]] <- single
      count_single <- count_single + 1
    }
  }
  
  if (length(multi$ID) != 0) {
    out_multi[[count_multi]] <- multi
    count_multi <- count_multi + 1
  }
}

out_single = rbindlist(out_single) %>% 
  as_tibble() %>% 
  st_as_sf()

out_multi = rbindlist(out_multi) %>% 
  as_tibble() %>% 
  st_as_sf()

end.time <- Sys.time()
end.time - start.time 

cl <- parallel::makeCluster(2)  
doParallel::registerDoParallel(cl)  

results2 <- foreach::foreach(id_ind = 1:40, .packages = c("tcltk", "sf", "dplyr", "tibble", "data.table" )) %dopar% {
  print(id_ind)
  id = out_multi$ID[id_ind]
  single_geom <- out_multi[which(out_multi$ID == id), ]
  all_others  <- out_multi[(-id_ind), ]
  inters_codes <- sf::st_intersects(single_geom, single_geom, sparse = TRUE) 
  for (k in 1:length(inters_codes)){
    inters_codes[[k]] <- inters_codes[[k]][which(inters_codes[[k]] != k)]
  }
  
  n_inters    <- length(which(inters_codes == TRUE))
  if (n_inters > 0) {
    intersections <- sf::st_intersection(single_geom, all_others)
    difference    <- sf::st_difference(single_geom, sf::st_union(intersections))
    intersections <- intersections[which(st_dimension(intersections) == 2),]
    if (length(intersections$ID) != 0 ) {
      if (any(st_is(intersections, "GEOMETRYCOLLECTION"))) {
        which_mixed        <- which(sf::st_is(intersections, "GEOMETRYCOLLECTION") == TRUE)
        newintersections   <- intersections[which(sf::st_is(intersections, "GEOMETRYCOLLECTION") == FALSE ), ] 
        int_split          <- intersections[which(sf::st_is(intersections, "GEOMETRYCOLLECTION") == TRUE ),  ] 
        if (any(sf::st_is(int_split ,"POLYGON") | sf::st_is(int_split ,"MULTIPOLYGON"))) {
          int_split        <- int_split[which(sf::st_is(int_split ,"POLYGON") | sf::st_is(int_split ,"MULTIPOLYGON")), ]
          attr(int_split, "sf_column") = "geoms"
          newintersections <- rbind(newintersections, int_split)
        } else {
          intersections    <- newintersections
        }
      }
      areas <- as.numeric(sf::st_area(intersections))/10000
      intersections <- intersections[which(sf::st_dimension(intersections) == 2),]
      intersections <- intersections[areas >= 0.5,]
    }
    if (length(intersections$ID) != 0 ) {
      # if (length(intersections$ID) != 0 ) {
      # # inters_codes  <- inters_codes[which(st_dimension(intersections) == 2)]
      # 
      # 
      # 
      #   #   ____________________________________________________________________________
      #   #   Identify intersecting geoms                                             ####
      #   area_ints <- as.numeric(sf::st_area(intersections)/10000)
      #   out_multis <- NULL
      #   for (i in seq_along(along = intersections$ID)) {
      #     # browser()
      #     int_single <- intersections[i,]
      #     n_int      <- (length(names(int_single)) + 1) / length(names(single_geom))
      #     int_ids    <- int_single[1, c(1, length(names(single_geom))*(1:(n_int - 1)))] %>% 
      #       tibble::as_data_frame() %>% 
      #       as.numeric() %>% 
      #       sort()
      #     if (length(intersect(int_ids, list_int_ids)) == 0) {
      #       # intersections[i,] <- st_cast(int_single, "MULTIPOLYGON")
      #       out_multi <- int_single  %>% 
      #         dplyr::mutate(FID_Burned = FID_Burned, overlap_ID = overlap_ID) %>% 
      #         dplyr::mutate(Area_Int = area_ints[i]) %>% 
      #         dplyr::select(FID_Burned, Area_Int, overlap_ID) %>% 
      #         sf::st_cast("MULTIPOLYGON") %>% 
      #         dplyr::mutate(int_ids = paste(int_ids, collapse = " "))
      #       
      #       if (i == 1) {
      #         out_multis <- out_multi
      #       } else {
      #         out_multis <- rbind(out_multis, out_multi)
      #       }
      #       # add lines to csv LUT data frame
      #       
      #       fire_dates  <- tibble::as_data_frame(int_single[1, c(2, 1 + length(names(single_geom))*(1:(n_int - 1)))])[1,1:n_int]
      #       placenames  <- as.character(tibble::as_data_frame(int_single[1, c(5, 4 + length(names(single_geom))*(1:(n_int - 1)))])[1,1:n_int])
      #       yearseasons <- as.character(tibble::as_data_frame(int_single[1, c(11, 10 + length(names(single_geom))*(1:(n_int - 1)))])[1,1:n_int])
      #       out_lut     <- rbind(out_lut, data.frame(OBJECTID   = int_ids,
      #                                                FireDate   = format(as.Date(as.character(fire_dates[1,])),  "%d-%m-%Y"), 	
      #                                                Place_Name = placenames,
      #                                                YearSeason = yearseasons,
      #                                                OVERLAP_ID = overlap_ID, 
      #                                                int_ids    = paste(int_ids, collapse = " ")))
      #       overlap_ID <- overlap_ID + 1
      #       list_int_ids <- rbind(list_int_ids, paste(int_ids, collapse = " "))  
      #     } 
      #   }
      #   
      # out_multis <- data.table::rbindlist(out_multis)
      # browser()
      
      
      # if (length(difference$ID != 0)) {
      # FID_Burned <- FID_Burned + 1 
      return(list(single  = sf::st_cast(difference, "MULTIPOLYGON"),
                  multi   = sf::st_cast(intersections, "MULTIPOLYGON")))
      # , 
      #      out_lut = out_lut))
    } else {
      return(list(single  = sf::st_cast(difference, "MULTIPOLYGON"),
                  multi   = NULL, 
                  out_lut = NULL))
      
    }
  } else {
    return(list(single  = sf::st_cast(single_geom, "MULTIPOLYGON"), 
                multi   = NULL, 
                out_lut = NULL))
  }
  # } else {
  #   return(list(single  = sf::st_cast(single_geom, "MULTIPOLYGON"), 
  #               multi   = NULL, 
  #               out_lut = NULL))
  # }
}  


#


# 

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
# 
# 
# 
# 
# 
# # %>% 
# #   as("sf")
# # # intersect   <- st_intersection(single_geom, crop_union)
# # diff        <- st_difference(crop_union, single_geom)
# # # all_others      <- st_difference(union_orig, single_geom)
# # # out_single_geom <- st_overlaps(single_geom, union_orig) 
# # # lgt = rbind(lgt, length(out_single_geom[[1]]))
# # print(st_bbox(diff))
# # if (!is.na(st_bbox(diff)[1])) {
# #   browser()
# #   out_single[[id_ind]] <- st_difference(single_geom, union_orig)
# #   out_multi[[id_ind]]  <- st_intersection(single_geom, union_orig)
# # } else {
# #   out_single[[id_ind]] <- single_geom
# # }
# # }
# # end.time <- Sys.time()
# # time.taken <- end.time - start.time
# # time.taken
# # 
# # out_single <- in_orig
# # st_geometry(out_single) <- out_single_geom
# # 
# # out_singlesp <- as(out_single, "Spatial")
# # 
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
# 
pr <- st_intersects(out_multi, out_multi)

out_multi = in_orig
single_inters = list()
for (n_intersects in 1:10) {
  print(n_intersects)
  # browser()
  pr <- st_intersects(out_multi, out_multi)  
  for (k in 1:length(pr)) {
    pr[[k]] <- pr[[k]][which(pr[[k]] != k)]
  }
  ninters <- unlist(lapply(pr, FUN = length))
  single_inters[[n_intersects]]  <- out_multi[ninters == (n_intersects - 1), ] %>% 
    filter(st_dimension(.) == 2)
  # <- out_single[which(st_dimension(out_single) == 2), ]
  out_multi <- out_multi[ninters > (n_intersects - 1), ] %>% 
    filter(st_dimension(.) == 2)
  # out_multi <- out_multi[which(st_dimension(out_multi) == 2), ]
  print(length(single_inters[[n_intersects]]$ID))
  print(length(out_multi$ID))
  if (length(out_multi$ID) == 0) break
}

small_feat = list()
count <- 1
for (n_intersects in 1:length(single_inters)) {
  print(n_intersects)
  feats <- single_inters[[n_intersects]]
  for (n_feat in 1:length(feats)) {
    small_feat[[count]] <- feats[n_feat,]
    count <- count + 1
  }
}

b = small_feat %>% 
  rbindlist() %>% 
  as_tibble() %>% 
  arrange(ID) %>%
  st_as_sf() %>% 
  filter(ID == 700)

ninters <- unlist(lapply(pr, FUN = length))
single_inters <- out_multi[ninters == 0, ]
out_multi <- out_multi[ninters > 0, ]

pr2 <- st_intersects(out_multi, out_multi)
for (k in 1:length(pr2)) {
  pr2[[k]] <- pr2[[k]][which(pr2[[k]] != k)]
}
ninters <- unlist(lapply(pr, FUN = length))
