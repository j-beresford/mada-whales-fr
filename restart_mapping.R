rm(list=ls())


mapping_old<-s3readRDS(object = "map.rds", bucket = "mada-whales")

#Make changes
map<-mapping_old%>%
  filter(!sighting_id %in% c("8b7c75d4-9d64"))




s3saveRDS(x = map, bucket = "mada-whales", object = "map.rds")


