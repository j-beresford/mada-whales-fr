rm(list=ls())


mapping_old<-s3readRDS(object = "map.rds", bucket = "mada-whales")

#Make changes
map<-mapping_old%>%
  filter(!is.na(sighting_id))%>%
  filter(i3s_id!="MD-432.jpg")%>%
  filter(!sighting_id %in% c("aa9d4349-c62a","066a7396-b9fd","cc9bd689-f519","3c38ad36-e2df"))




s3saveRDS(x = map, bucket = "mada-whales", object = "map.rds")


