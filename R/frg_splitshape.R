adm <- getData("GADM", country="ITA", level=1) %>% 
  as("sf") %>% 
  filter(NAME_1 =="Sardegna") %>% 
  st_transform(3035)

st_crs(adm) = st_crs (st_read("D:/Temp/tempfrg/Input_Shapefiles/Burned_Areas_00_15.shp"))

in_orig    <- "D:/Temp/tempfrg/Input_Shapefiles/Burned_Areas_00_15.shp" %>% 
  st_read(stringsAsFactors = FALSE)   %>% 
   st_intersection(adm)
tmp = in_orig
valid = st_is_valid(in_orig)
if (max(valid) != 0) {
  in_orig[which(valid == FALSE),] = st_buffer(in_orig[which(valid == FALSE),], 0.01)
}
# 
start.time <- Sys.time()
union_orig <- st_union (in_orig)

lgt = NA
out_single = list()
out_multi = list()
for (id_ind in seq_along(along.with = in_orig$ID)) {
  # print(id_ind)
  id = in_orig$ID[id_ind]
  single_geom <- st_cast(in_orig[which(in_orig$ID == id), ], "MULTIPOLYGON")
  intersect   <- st_intersection(single_geom, union_orig)
  diff        <- st_difference(intersect, single_geom)
  # all_others      <- st_difference(union_orig, single_geom)
  # out_single_geom <- st_overlaps(single_geom, union_orig) 
  # lgt = rbind(lgt, length(out_single_geom[[1]]))
  print(st_bbox(diff))
  if (length(diff$ID) > 0) {
    browser()
    out_single[[id_ind]] <- st_difference(single_geom, union_orig)
    out_multi[[id_ind]]  <- st_intersection(single_geom, union_orig)
  } else {
    out_single[[id_ind]] <- single_geom
  }
}
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

out_single <- in_orig
st_geometry(out_single) <- out_single_geom

out_singlesp <- as(out_single, "Spatial")

out_multi <- st_union(in_orig) %>% 
             st_difference(in_orig)






in_single  <- "D:/Temp/tempfrg/Intermed_Proc/Shapefiles/Burned_Areas_00_15_Single_Fires.shp" %>% 
  st_read(stringsAsFactors = FALSE)  %>% 
  st_intersection(adm) %>% 
  as("Spatial")

in_multi   <- "D:/Temp/tempfrg/Intermed_Proc/Shapefiles/Burned_Areas_00_15_Multiple_Fires.shp" %>% 
  st_read() %>% 
  st_intersection(adm) 

  in_multi <- arrange(in_multi, FID_Burned)


mapview(as(in_single, "Spatial"), zcol = "ID")+
  mapview(as(in_multi, "Spatial"), zcol = "FID_Burned") +
  mapview(as(in_orig, "Spatial"), zcol = "ID")

pro = st_difference(in_orig, st_union(in_orig))

val  = st_make_valid(in_orig)

names(in_orig)
names(in_single)
names(in_multi)
