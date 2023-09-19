url<-"https://kf.kobotoolbox.org/api/v2/assets/aJ5NwkApvziLAUE7i9eHcn/data.json"

u<-"madawhale"
pw<-"Wh6l3Sh6rk"

# Call form_meta API and Parse JSON
rawdata<-GET(url,authenticate(u,pw),progress())
p<-jsonlite::parse_json(rawdata)
results<-p$results

tablet_ids=data.frame(
  tablet_name=c("Justin Mobile","Vert","Orange","Bleu","Amy Laptop","Justin Laptop","ClareEloy","Extra"),
  client_identifier=c("collect:8FdYHpOdn4NijbBq",
                   "collect:k0YytH7Ux8Qzx8C0",
                   "collect:LA3tDYnylZq5mFlv",
                   "collect:GanYDasis6QSzhUT",
                   "ee.kobotoolbox.org:JWGByiliaVweMF0i",
                   "ee.kobotoolbox.org:xCPqKcFl8GV69TsD",
                   "collect:W6Y1Q3ywxp1Fb3lf",
                   "collect:fYgZOaqQBK4sGTrI"))

numbers=c("meteo","sst","sea_state","trichodesmium_pct")

df<-tibble(list_col=results)%>%
  hoist(list_col,'sighting_repeat')%>%
  hoist(list_col,'_attachments')%>%
  hoist(list_col,'_geolocation')%>%
  hoist(list_col,'_tags')%>%
  hoist(list_col,'_notes')%>%
  hoist(list_col,'_validation_status')%>%
  unnest_wider('_geolocation', names_repair = "unique")%>%
  unnest_wider(list_col, names_repair = "unique")%>%
  rename("trip_id"="_id")%>%
  select(-'Faune/trichodesmium_pct')%>%
  rename_with(~str_remove(., 'Faune/'))%>%
  left_join(tablet_ids,by="client_identifier")%>%
  mutate_at(numbers,as.numeric)%>%
  mutate_if(is.character,as.factor)%>%
  mutate(trip_id=as_factor(trip_id))

sighting_numbers<-c("sighting_number","size","boats_min","boats_max")

all_sightings=df%>%
  unnest_longer(sighting_repeat)%>%
  unnest_wider(sighting_repeat)%>%
  rename_with(~str_remove(., 'sighting_repeat/'))%>%
  rename(sighting_id=shark_uuid)%>%
  mutate(sighting_id=str_remove_all(sighting_id,"uuid:"))%>%
  mutate(sighting_id=str_sub(sighting_id,1,13))%>%
  mutate_at(numbers,as.numeric)


shark_sightings=all_sightings%>%
  filter(megaf_or_shark=="shark")

megaf_sightings=all_sightings%>%
  filter(megaf_or_shark=="megaf")


shark_scar_sightings<-shark_sightings%>%
  filter(scar_number!='NULL')%>%
  unnest_longer(scar_number, names_repair = "unique")%>%#
  unnest_wider(scar_number,names_repair="unique")%>%
  rename_with(~str_remove(., 'sighting_repeat/scar_number/'))
