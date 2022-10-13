rm(list=ls())





mapping_old<-s3readRDS(object = "map.rds", bucket = "mada-whales")

map<-mapping_old%>%
  filter(!sighting_id %in% c("asdfasdfad"))

s3saveRDS(x = map, bucket = "mada-whales", object = "map.rds")  

