url<-"https://kf.kobotoolbox.org/api/v2/assets/aJ5NwkApvziLAUE7i9eHcn/data.json"

# Call form_meta API and Parse JSON
rawdata<-GET(url,authenticate(u,pw),progress())
p<-jsonlite::parse_json(rawdata)
results<-p$results