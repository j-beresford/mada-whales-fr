url<-"https://kf.kobotoolbox.org/api/v2/assets/aJ5NwkApvziLAUE7i9eHcn/data.json"

# Call form_meta API and Parse JSON
rawdata<-GET(url,authenticate(u,pw),progress())
p<-jsonlite::parse_json(rawdata)
results<-p$results

maps_test<-data.frame(a=c(1,2,3))


aws.s3::put_object(file = "maps_test",object="maps_test",bucket="mada-whales")