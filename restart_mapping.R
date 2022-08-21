rm(list=ls())

map<-data.frame('sighting_id'=NA,
                'i3s_id'=NA,
                'no_id_reason'=NA)

s3saveRDS(x = map, bucket = "mada-whales", object = "map.rds")

s3readRDS(object = "map.rds", bucket = "mada-whales")

