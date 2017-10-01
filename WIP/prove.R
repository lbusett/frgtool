library(tibble)
library(dplyr)
library(sf)
library(data.table)

ncircles <- 9
rmax     <- 120
x_limits <- c(-70,70)
y_limits <- c(-30,30)
set.seed(100) 
xy <- data.frame(
  id = paste0("id_", 1:ncircles), 
  x = runif(ncircles, min(x_limits), max(x_limits)),
  y = runif(ncircles, min(y_limits), max(y_limits))) %>% 
  as_tibble()

polys <- st_as_sf(xy, coords = c(2,3)) %>% 
  st_buffer(runif(ncircles, min = 1, max = 20)) 

plot(polys[1])
noint = list()
count_no = 1
int = list()
count_int = 1
polys = as(polys, "Spatial")


clip  <- gIntersection(polys[5,], polys[-5,], byid = TRUE, drop_lower_td = TRUE)
clip2 <- gIntersection(clip[1,], clip[-1,], byid = TRUE, drop_lower_td = FALSE)
int1  <- gDifference(clip, clip2, byid = T)
sing1  <- gDifference(polys[5,], clip, byid = F)


for (i in 1:9) {
  pol_i <- polys[i,]  
  other_pol <- polys[-i,]
  inters <- st_intersection(pol_i, other_pol)
  inters_2 <- st_intersection(inters, inters)
  if (length(inters$id) == 1) {
    no <-  st_difference(pol_i, inters)
    noint[[count_no]] <- no
    int[[count_int]]  <- inters
    count_int = count_int + 1
    count_no = count_no + 1
    polys  <- st_difference(other_pol, inters) %>% st_cast()
    # new2 <- st_difference(new, noint)
  }
}
noint <- rbindlist(noint)  %>% as_tibble %>% st_as_sf()
int = rbindlist(int)  %>% as_tibble %>% st_as_sf()
