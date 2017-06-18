library(tidyverse)
library(sf)

ncircles <- 25
offset = NULL
rmax     <- 120
x_limits <- c(-120,120)
y_limits <- c(-60,60)
seed = 100
xy <- data.frame(
  id = paste0("id_", 1:25), 
  x = runif(ncircles, min(x_limits), max(x_limits)),
  y = runif(ncircles, min(y_limits), max(y_limits))) %>% 
  as_tibble()

polys <- st_as_sf(xy, coords = c(2,3)) %>% 
  st_buffer(runif(25, min = 1, max = 20)) %>% 
  st_set_crs(4326) %>% 
  as("Spatial")

aaa <- (SpatialPolygons2PolySet(polys))
bb = combinePolys(aaa)


int.armadillo <- combinePolys(joinPolys(aaa, aaa, "INT"))

plotPolys(int.armadillo, col=int.armadillo$PID, lwd=2)

                 SpatialPolygons2PolySet(piece), operation="INT")

ggplot(polys) + geom_sf(aes(color = id), fill = "transparent")




single_inters = list()
n_intersects = 1
p = in_orig[1,]
# intersections <- sf::st_intersection(p, all_others)

 int2 <- suppressWarnings(p %>% 
   sf::st_intersection(in_orig) %>%
   mutate(area = st_area(.)) %>% 
   dplyr::filter(st_dimension(.) == 2) %>% 
   arrange(as.numeric(area)) %>% 
   filter(as.numeric(area) > 5000) %>% 
   st_cast("MULTIPOLYGON"))
 
 plot(p[1])
 small_feat = list()
 intnew = int2
 int = p
 k = 1
 # for (k in 1:(dim(int2)[1])) {
 int <- suppressWarnings(int %>% 
                              sf::st_intersection(in_orig) %>%
                              mutate(area = st_area(.)) %>% 
                              dplyr::filter(st_dimension(.) == 2) %>% 
                              arrange(as.numeric(area)) %>% 
                              filter(as.numeric(area) > 5000) %>% 
                              st_cast("MULTIPOLYGON"))  
   while (dim(int)[1] > 0) {

   # while(dim(int2)[1] > 0 )
    # int_geom = suppressWarnings(st_cast(intnew[1,], "POLYGON") %>% 
    #               mutate(n_int = k))
   # if (dim(int_geom)[1] != 0){
     
     small_feat[[k]] <- int[1,]
    
     int <- suppressWarnings(st_difference(int,int[1,]))
     plot(int[1,], add = T, col = "purple")
     k = k +1
   }
   
   
   
   } else {
     break
   }
 }
 out = rbindlist(small_feat) %>% st_as_sf()  
   # plot(int2[1])
 
   
pr <- st_intersects(p, all_others)  
for (k in 1:length(pr)) {
  pr[[k]] <- pr[[k]][which(pr[[k]] != k)]
}
ninters <- unlist(lapply(pr, FUN = length))
int2$ninters <- ninters


single_inters[[n_intersects]]  <- int2[ninters == (n_intersects - 1), ] %>% 
  filter(st_dimension(.) == 2)
# <- out_single[which(st_dimension(out_single) == 2), ]
out_multi <- out_multi[ninters > (n_intersects - 1), ] %>% 
  filter(st_dimension(.) == 2)
# out_multi <- out_multi[which(st_dimension(out_multi) == 2), ]
print(length(single_inters[[n_intersects]]$ID))
print(length(out_multi$ID))
if (length(out_multi$ID) == 0) break

dput(p)
dput(intersects)

mypolys <-