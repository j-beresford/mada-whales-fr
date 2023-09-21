daily_dives_sst<-displayTrip(trip_vars)%>%
  mutate(year=year(date))%>%
  ggplot(aes(year,fill=operator))+
  geom_bar()+
  theme_minimal()+
  theme(text = element_text(size=14),
        panel.grid = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  labs(y="Annual Expeditions",x="",fill="Operator")+
  scale_fill_brewer(type="qual",palette = "Pastel1")
  

sightings_sex <-mapUpdateUniqueTripSightings()%>%
  mutate(`Sex (mode)`=if_else(is.na(`Sex (mode)`),"Undetermined",`Sex (mode)`))%>%
  mutate(year=year(Date))%>%
  ggplot(aes(year,fill=`Sex (mode)`))+
  geom_bar()+
  theme_minimal()+
  theme(text = element_text(size=14),
        panel.grid = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  scale_fill_brewer(type="qual",palette = "Pastel1")+
  labs(y="Unique annual sightings",x="")


sharks_by_size_sex<-mapUpdateUniqueYearlySightings()%>%
  mutate(`Sex (mode)`=if_else(is.na(`Sex (mode)`),"Undetermined",`Sex (mode)`))%>%
  filter(!Year %in% c(2021,2023))%>%
  filter(`Size (mean)`>0)%>%
  ggplot(aes(`Size (mean)`,`Sex (mode)`,fill=`Sex (mode)`))+
  geom_boxplot(show.legend = FALSE)+
  theme_minimal()+
  labs(x="Size (meters)",y="")+
  facet_wrap(.~Year,ncol=1)+
  scale_fill_brewer(type="qual",palette = "Pastel1")

megaf_all<-megaf_sightings%>%
  mutate(espece=str_replace_all(espece,"_"," "))%>%
  filter(!is.na(espece))%>%
  mutate(espece=str_to_title(espece))%>%
  mutate(date=as_date(survey_start))%>%
  mutate(year=year(date))%>%
  ggplot(aes(year,fill=espece))+
  geom_bar()+
  theme_minimal()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  labs(x="",y="Number of sightings (annual)",fill="Species")


distributions<-displayTrip(trip_vars)%>%
  select(all_sightings,sst,meteo,sea_state,trichodesmium_pct)%>%
  rename(sea_surface_temperature=sst)%>%
  gather(key=var,value=val,-all_sightings)%>%
  ggplot(aes(val))+
  geom_density(show.legend = FALSE, fill="#00BCFF")+
  facet_wrap(.~var, scales = "free")+
  labs(x="",y="Expeditions,kernel density")+
  theme_minimal()+
  theme(legend.position = "none",
        strip.background = element_rect(fill="#00BCFF",color="white"),
        strip.text = element_text(color="white",size=14),
        text = element_text(size=14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
  




map<-map_data("world","Madagascar")%>%
  filter(!subregion  %in% c("Ile Sainte-Marie"))%>%
  mutate(subregion=if_else(is.na(subregion),"Mainland",subregion))%>%
  mutate(subregion=fct_reorder(subregion,lat,mean))%>%
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill=subregion))+
  theme(panel.background = element_rect(fill = "lightblue"),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())+
  coord_fixed(1.3)+
  scale_fill_manual(values = c("darkgreen","red"))+
  labs(fill="",color="",x="",y="")

megaf_cords=megaf_sightings%>%
  filter(!is.na(megaf_geo))%>%
  mutate(lat=word(megaf_geo,1))%>%
  mutate(long=word(megaf_geo,2))%>%
  mutate(group=1)%>%
  select(espece,group,megaf_count,megaf_number,long,lat)%>%
  mutate(espece=factor(espece))%>%
  mutate(long=as.numeric(long))%>%
  mutate(lat=as.numeric(lat))

megaf_map<-map+geom_point(data=megaf_cords, aes(x=long, y=lat,color=espece),
                          alpha=0.5,size=1)
megaf_map<-ggplotly(megaf_map)%>%layout(height=600,width=600)

megaf_density<-map+geom_density2d(data=megaf_cords,aes(x=long, y=lat))
megaf_density<-ggplotly(megaf_density)%>%layout(height=600,width=600)


shark_cords=shark_sightings%>%
  filter(!is.na(shark_geo))%>%
  mutate(lat=word(shark_geo,1))%>%
  mutate(long=word(shark_geo,2))%>%
  mutate(group=1)%>%
  select(sex,size,scars,code_of_conduct,long,lat,group)%>%
  mutate(long=as.numeric(long))%>%
  mutate(lat=as.numeric(lat))

shark_map<-map+geom_point(data=shark_cords, aes(x=long, y=lat,color=sex),
                          alpha=0.5,size=1)
shark_map<-ggplotly(shark_map)%>%layout(height=600,width=600)

shark_density<-map+geom_density2d(data=shark_cords,aes(x=long, y=lat))
shark_density<-ggplotly(shark_density)%>%layout(height=600,width=600)