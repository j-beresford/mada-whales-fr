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


sex_ratio <- mapUpdateUniqueYearlySightings()%>%
  group_by(Year, `Sex (mode)`)%>%
  summarise(sharks=n())%>%
  spread(key=`Sex (mode)`,value=sharks)%>%
  mutate(sex_ratio_male_female = male/female)%>%
  ggplot(aes(Year,sex_ratio_male_female))+
  geom_line(color="#00BCFF",lwd=0.8)+
  geom_point(color="#00BCFF")+
  theme_minimal()+
  scale_y_continuous(limits = c(0,11.5))+
  labs(x="",y="Sex ratio, male-to-female")




sightings_sex <-mapUpdateUniqueYearlySightings()%>%
  mutate(`Sex (mode)`=if_else(is.na(`Sex (mode)`),"Undetermined",`Sex (mode)`))%>%
  ggplot(aes(Year,fill=`Sex (mode)`))+
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






#### total mess alert ###
yearly<-mapUpdateUniqueYearlySightings()%>%
  rename(md=`I3S ID`)%>%
  select(Year,md)%>%
  arrange(md,Year)

ret_rate_finder<-function(base_year){
  
  result_df <- data.frame()
  
  for (cur_year in base_year:2023){
    ids_base = yearly%>%filter(Year==base_year)%>%pull(md)
    ids_current = yearly%>%filter(Year==cur_year)%>%pull(md)
    retention = mean(ids_current %in% ids_base)
    
    new_data = data.frame(year=cur_year,retention_rate=retention)
    result_df = rbind(result_df,new_data)
    
  }
  
  return(result_df)
}

a<-ret_rate_finder(2015)%>%rename("2015"=retention_rate)
b<-ret_rate_finder(2016)%>%rename("2016"=retention_rate)
c<-ret_rate_finder(2017)%>%rename("2017"=retention_rate)
d<-ret_rate_finder(2018)%>%rename("2018"=retention_rate)
e<-ret_rate_finder(2019)%>%rename("2019"=retention_rate)
h<-ret_rate_finder(2022)%>%rename("2022"=retention_rate)
i<-ret_rate_finder(2023)%>%rename("2023"=retention_rate)

ret_rates<-a%>%
  full_join(b,on=year)%>%
  full_join(c,on=year)%>%
  full_join(d,on=year)%>%
  full_join(e,on=year)%>%
  full_join(h,on=year)%>%
  full_join(i,on=year)

recurrence_rates <- ret_rates%>%
  gather(key = period,value=retention,-year)%>%
  mutate(year=as.numeric(year))%>%
  filter(!year %in% c(2020))%>%
  ggplot(aes(year,retention*100,color=period))+
  geom_line(lwd=0.8, show.legend = FALSE)+
  theme_minimal()+
  labs(x="",y="Recurrence rate by year")



new_sharks_by_year <- yearly%>%
  group_by(md)%>%
  mutate(new=if_else(row_number()>1,0,1))%>%
  ungroup()%>%group_by(Year)%>%summarise(new_rate = mean(new))%>%
  ggplot(aes(Year,new_rate*100))+
  geom_line(color="#00BCFF", lwd=0.8)+
  scale_y_continuous(limits=c(0,100))+
  labs(x="",y="% of sharks that are new this year")+
  theme_minimal()
