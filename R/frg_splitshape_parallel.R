library(tidyverse)
library(sf)
library(raster)
# 
# adm <- getData("GADM", country="ITA", level=1) %>% 
#   as("sf") %>% 
#   filter(NAME_1 =="Sardegna") %>% 
#   st_transform(3035)

# st_crs(adm) = st_crs(st_read("/home/lb/Google_Drive/FRG/Input_Shapefiles/Burned_Areas_00_15.shp"))

in_orig    <- "/home/lb/Google_Drive/FRG/Input_Shapefiles/Burned_Areas_00_15.shp" %>% 
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
# in_orig <- in_orig %>% 
#   arrange(geometry)
# union_orig <- st_union(in_orig) %>% 
#   as("Spatial")



# all_features  <- st_union(st_cast(in_orig[which(in_orig$ID != id), ], "MULTIPOLYGON"))

start.time <- Sys.time()
cl <- parallel::makeCluster(4)  
doParallel::registerDoParallel(cl)  

n_IDs = length(in_orig$ID)
# pb <- tcltk::tkProgressBar("Parallel task", min = 1, max = 100)
results <- foreach::foreach(id_ind = 1:100, .packages = c("tcltk", "sf")) %dopar% {
  
  if (!exists("pb2")) {
    pb2 <- tcltk::tkProgressBar("Parallel task", min = 1, max = 100)
  }
  tcltk::setTkProgressBar(pb2, id_ind)
  Sys.sleep(0.05)
  
  id = in_orig$ID[id_ind]
  single_geom <- in_orig[which(in_orig$ID == id), ]
  all_others  <- in_orig[which(in_orig$ID != id), ]
  # diff_all    <- st_difference(all_features, single_geom)
  inters      <- st_intersects(single_geom, all_others, sparse = FALSE)
  n_inters    <- sum(inters, na.rm = T)
  # if (id == 28247) browser()
  if (n_inters > 0) {
    
    intersections <- st_intersection(single_geom, all_others)
    intersections <- intersections[which(st_dimension(intersections) == 2),] %>% 
      select(c(1:22, geoms))
    
    if (length(intersections$ID) != 0 ) {
      
      if (any(st_is(intersections, "GEOMETRYCOLLECTION"))) {
        which_mixed        <- which(st_is(intersections, "GEOMETRYCOLLECTION") == TRUE)
        newintersections   <- intersections[which(st_is(intersections, "GEOMETRYCOLLECTION") == FALSE ), ] 
        int_split          <- intersections[which(st_is(intersections, "GEOMETRYCOLLECTION") == TRUE ),  ] 
        if (any(st_is(int_split ,"POLYGON") | st_is(int_split ,"MULTIPOLYGON"))) {
          int_split        <- int_split[which(st_is(int_split ,"POLYGON") | st_is(int_split ,"MULTIPOLYGON")), ]
          attr(int_split, "sf_column") = "geoms"
          newintersections <- rbind(newintersections, int_split)
        } else {
          intersections    <- newintersections
        }
      }
      
      intersections <- intersections[which(st_dimension(intersections) == 2),]
      if (length(intersections$ID) != 0 ) {
        for (i in seq_along(intersections$id)) {
          intersections[i,] <- st_cast(intersections[i,], "MULTIPOLYGON")
        }
        difference    <- st_difference(single_geom, st_union(intersections))
        
        if (length(difference$ID != 0)) {
          return(list(single = st_cast(difference, "MULTIPOLYGON"), multi = st_cast(intersections, "MULTIPOLYGON")))
        }
        
      } else {
        return(list(single = st_cast(single_geom, "MULTIPOLYGON"), multi = NULL)) 
        
      }
    } else {
      return(list(single = st_cast(single_geom, "MULTIPOLYGON"), multi = NULL))
    }
  } else {
    return(list(single = st_cast(single_geom, "MULTIPOLYGON"), multi = NULL))
  }
}  
stopCluster(cl)
end.time <- Sys.time()
end.time - start.time 

out_single = list()
out_multi = list()
count_single <- 1
count_multi  <- 1

for (i in 1: 100) {
  
  single <- results[[i]]$single
  if (length(multi$ID) != 0) {
    out_single[[i]] <- results[[i]]$single
    count_single <- count_single +1
  }
  
  multi <- results[[i]]$multi
  if (length(multi$ID) != 0) {
    out_multi[[count_multi]] <- multi
    count_multi <- count_multi +1
  }
  
}


out_single_all <- out_single %>% 
  data.table::rbindlist() %>% 
  as_tibble() %>% 
  arrange(ID) %>% 
  st_as_sf() 

out_multi_all <- out_multi %>% 
  data.table::rbindlist() %>% 
  as_tibble() %>% 
  arrange(ID) %>% 
  st_as_sf() 





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
