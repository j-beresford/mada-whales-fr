##############################################
############### Raw Data #####################
##############################################

################## Trip ######################
displayTrip <- function(vars){
  trip_display = df %>%
    mutate(date=as_date(survey_start))%>%
    mutate(all_sightings=lengths(sighting_repeat))%>%
    select(any_of(vars))
  return(trip_display)
}
############### Megafauna #####################
displayMegaf <- function(vars){
  megaf_display = megaf_sightings %>%
    mutate(date=as_date(survey_start))%>%
    filter(megaf_or_shark=='megaf') %>%
    select(any_of(vars))%>%
    arrange(desc(date))
  return(megaf_display)
}
############# Shark scars data ###############
displaySharkScars <- function(vars){
  shark_scars_display<-shark_scar_sightings%>%
    select(any_of(vars))%>%
    mutate(survey_start=as_date(survey_start))%>%
    rename(date=survey_start)%>%
    arrange(desc(date))
  return(shark_scars_display)
}

########### shark sightings #############
displaySharkSightings <- function(vars){
  shark_sightings_display<-all_sightings%>%
    mutate(survey_start=as_date(survey_start))%>%
    mutate(survey_end=as_date(survey_end))%>%
    filter(megaf_or_shark %in% c("shark", "chasse") & !is.na(sighting_id))%>%
    select(any_of(vars))%>%
    arrange(desc(survey_start))
  shark_sightings_display
}
##############################################
############### Classifiers ##################
##############################################

mapUpdateUNClassified <- function() {
  mapping<-s3readRDS(object = "map.rds", bucket = "mada-whales")
  uc<-shark_sightings%>%
    full_join(mapping,by="sighting_id")%>%
    filter(left_id=="yes")%>%
    group_by(sighting_id)%>%
    mutate(t=n())%>%
    ungroup()%>%
    filter(t==1)%>%
    filter(is.na(i3s_id)|i3s_id=="")%>%
    filter(!no_id_reason %in% c("unusable_sighting"))%>%
    mutate(date=as_date(survey_start))%>%
    select(any_of(map_unclassified_vars))%>%
    rename(notes = surrounding_objects)
  return(uc)
}


mapUpdateUnusable <- function() {
  mapping<-s3readRDS(object = "map.rds", bucket = "mada-whales")
  shark_sightings%>%
    full_join(mapping,by="sighting_id")%>%
    mutate(date=as_date(survey_start))%>%
    filter(no_id_reason %in% c("unusable_sighting")|left_id=="no")%>%
    mutate(no_id_reason=if_else(left_id=="no","No left ID",no_id_reason))%>%
    select(any_of(map_unusable_vars))
}


mapUpdateClassified <- function(vars) {
  mapping<-s3readRDS(object = "map.rds", bucket = "mada-whales")
  shark_sightings%>%
    full_join(mapping,by="sighting_id")%>%
    filter(str_detect(i3s_id, "^MD-\\d{3}"))%>%
    mutate(date=as_date(survey_start))%>%
    select(any_of(vars))
}

mapUpdateScars <- function(){
  known<-mapUpdateClassified(map_classified_vars)
  scars<-displaySharkScars(shark_scar_vars)
  test<-scars%>%
    full_join(known,by=c("sighting_id","tablet_name","observer",
                         "trip_id","date","scars"))%>%
    arrange(desc(date))%>%
    filter(scars=="yes")%>%
    select(-shark_name_known,-sighting_number,-tablet_name,-observer,
           -trip_id,-operator,-scars)
}

is_not_allowed <- function() {
  mapping<-s3readRDS(object = "map.rds", bucket = "mada-whales")
  not_allowed=mapping%>%
    filter(!no_id_reason %in% c("advice_needed"))
  return(not_allowed)
}


mapUpdateKnownSharks <- function() {
  mapping<-s3readRDS(object = "map.rds", bucket = "mada-whales")
  unique_sharks<-mapping%>%
    filter(!no_id_reason %in% c("advice_needed","unusable_sighting"))%>%
    full_join(shark_sightings,by="sighting_id")%>%
    filter(!is.na(i3s_id))%>%
    select(i3s_id,size,sex,scars,left_id,right_id,tag,drone,prey,survey_start)%>%
    mutate(size=as.numeric(size))%>%
    mutate(size=round(size,2))%>%
    mutate(survey_start=as_date(survey_start))%>%
    group_by(i3s_id)%>%
    mutate("First sighting" = as_factor(year(min(survey_start))))%>%
    mutate("Total sightings"=n())%>%
    mutate(survey_start=min(survey_start))%>%
    mutate(scars=if_else(sum(scars=="yes",na.rm=TRUE)>0,"yes","no"))%>%
    mutate(left_id=if_else(sum(left_id=="yes",na.rm=TRUE)>0,"yes","no"))%>%
    mutate(right_id=if_else(sum(right_id=="yes",na.rm=TRUE)>0,"yes","no"))%>%
    mutate(tag=sum(tag=="yes",na.rm=TRUE))%>%
    mutate(drone=sum(drone=="yes",na.rm=TRUE))%>%
    mutate(prey=sum(prey=="yes",na.rm=TRUE))%>%
    mutate(size=round(mean(size,na.rm=TRUE),2))%>%
    mutate(sex=case_when(
      mean(sex=="male",na.rm=TRUE)>mean(sex=="female",na.rm=TRUE)~"male",
      mean(sex=="male",na.rm=TRUE)<mean(sex=="female",na.rm=TRUE)~"female",
      mean(sex=="male",na.rm=TRUE)==mean(sex=="female",na.rm=TRUE)~"Undetermined"))%>%
    ungroup()%>%
    distinct()%>%
    rename("I3S ID"=i3s_id,"Size (mean)"=size,"Sex (mode)"=sex,
           "Identified scars"=scars,
           "Left ID"=left_id,"Right ID"=right_id,
           "Tag count"=tag,"Drone measurements"=drone,"Prey samples"=prey)%>%
    select("First sighting", everything())
  return(unique_sharks)
}


mapUpdateUniqueTripSightings <- function() {
  mapping<-s3readRDS(object = "map.rds", bucket = "mada-whales")
  unique_sharks<-mapping%>%
    filter(!no_id_reason %in% c("advice_needed","unusable_sighting"))%>%
    full_join(shark_sightings,by="sighting_id")%>%
    filter(!is.na(i3s_id))%>%
    mutate(date=as_date(survey_start))%>%
    select(date,i3s_id,size,sex,scars,left_id,right_id,tag,drone,prey)%>%
    mutate(size=as.numeric(size))%>%
    group_by(i3s_id,date)%>%
    mutate("Daily sightings"=n())%>%
    mutate(scars=if_else(sum(scars=="yes",na.rm=TRUE)>0,"yes","no"))%>%
    mutate(left_id=if_else(sum(left_id=="yes",na.rm=TRUE)>0,"yes","no"))%>%
    mutate(right_id=if_else(sum(right_id=="yes",na.rm=TRUE)>0,"yes","no"))%>%
    mutate(tag=sum(tag=="yes",na.rm=TRUE))%>%
    mutate(drone=sum(drone=="yes",na.rm=TRUE))%>%
    mutate(prey=sum(prey=="yes",na.rm=TRUE))%>%
    mutate(size=round(mean(size,na.rm=TRUE),2))%>%
    mutate(sex=case_when(
      mean(sex=="male",na.rm=TRUE)>mean(sex=="female",na.rm=TRUE)~"male",
      mean(sex=="male",na.rm=TRUE)<mean(sex=="female",na.rm=TRUE)~"female",
      mean(sex=="male",na.rm=TRUE)==mean(sex=="female",na.rm=TRUE)~"Undetermined"))%>%
    ungroup()%>%
    distinct()%>%
    rename(Date=date,"I3S ID"=i3s_id,"Size (mean)"=size,"Sex (mode)"=sex,
           "Identified scars"=scars,"Left ID"=left_id,"Right ID"=right_id,
           "Tag count"=tag,"Drone measurents"=drone,"Prey samples"=prey)
  return(unique_sharks)
}


mapUpdateUniqueYearlySightings <- function() {
  mapping<-s3readRDS(object = "map.rds", bucket = "mada-whales")
  unique_yearly_sharks<-mapping%>%
    filter(!no_id_reason %in% c("advice_needed","unusable_sighting"))%>%
    full_join(shark_sightings,by="sighting_id")%>%
    filter(!is.na(i3s_id))%>%
    mutate(year=year(as_date(survey_start)))%>%
    select(year,i3s_id,size,sex,scars,left_id,right_id,tag,drone,prey)%>%
    mutate(size=as.numeric(size))%>%
    group_by(i3s_id,year)%>%
    mutate("Annual sightings"=n())%>%
    mutate(scars=if_else(sum(scars=="yes",na.rm=TRUE)>0,"yes","no"))%>%
    mutate(left_id=if_else(sum(left_id=="yes",na.rm=TRUE)>0,"yes","no"))%>%
    mutate(right_id=if_else(sum(right_id=="yes",na.rm=TRUE)>0,"yes","no"))%>%
    mutate(tag=sum(tag=="yes",na.rm=TRUE))%>%
    mutate(drone=sum(drone=="yes",na.rm=TRUE))%>%
    mutate(prey=sum(prey=="yes",na.rm=TRUE))%>%
    mutate(size=round(mean(size,na.rm=TRUE),2))%>%
    mutate(sex=case_when(
      mean(sex=="male",na.rm=TRUE)>mean(sex=="female",na.rm=TRUE)~"male",
      mean(sex=="male",na.rm=TRUE)<mean(sex=="female",na.rm=TRUE)~"female",
      mean(sex=="male",na.rm=TRUE)==mean(sex=="female",na.rm=TRUE)~"Undetermined"))%>%
    ungroup()%>%
    distinct()%>%
    rename(Year=year,"I3S ID"=i3s_id,"Size (mean)"=size,"Sex (mode)"=sex,
           "Identified scars"=scars,"Left ID"=left_id,"Right ID"=right_id,
           "Tag count"=tag,"Drone measurents"=drone,"Prey samples"=prey)
  return(unique_yearly_sharks)
}

mapUpdateUniqueWeeklySightings <- function() {
  mapping<-s3readRDS(object = "map.rds", bucket = "mada-whales")
  unique_weekly_sharks<-mapping%>%
    filter(!no_id_reason %in% c("advice_needed","unusable_sighting"))%>%
    full_join(shark_sightings,by="sighting_id")%>%
    filter(!is.na(i3s_id))%>%
    mutate(week=floor_date(as_date(survey_start),"weeks",week_start = 1))%>%
    select(week,i3s_id,size,sex,scars,left_id,right_id,tag,drone,prey)%>%
    mutate(size=round(as.numeric(size),2))%>%
    group_by(i3s_id,week)%>%
    mutate("Total weekly sightings"=n())%>%
    mutate(scars=if_else(sum(scars=="yes",na.rm=TRUE)>0,"yes","no"))%>%
    mutate(left_id=if_else(sum(left_id=="yes",na.rm=TRUE)>0,"yes","no"))%>%
    mutate(right_id=if_else(sum(right_id=="yes",na.rm=TRUE)>0,"yes","no"))%>%
    mutate(tag=sum(tag=="yes",na.rm=TRUE))%>%
    mutate(drone=sum(drone=="yes",na.rm=TRUE))%>%
    mutate(prey=sum(prey=="yes",na.rm=TRUE))%>%
    mutate(size=mean(size,na.rm=TRUE))%>%
    mutate(sex=case_when(
      mean(sex=="male",na.rm=TRUE)>mean(sex=="female",na.rm=TRUE)~"male",
      mean(sex=="male",na.rm=TRUE)<mean(sex=="female",na.rm=TRUE)~"female",
      mean(sex=="male",na.rm=TRUE)==mean(sex=="female",na.rm=TRUE)~"Undetermined"))%>%
    ungroup()%>%
    distinct()%>%
    rename("Week start"=week,"I3S ID"=i3s_id,"Size (mean)"=size,"Sex (mode)"=sex,
           "Identified scars"=scars,"Left ID"=left_id,"Right ID"=right_id,
           "Tag count"=tag,"Drone measurents"=drone,"Prey samples"=prey)
  return(unique_weekly_sharks)
}


get_summary_stats<-function(df){
  summary_stats<-df%>%
    mutate(Year="All Years")%>%
    group_by(Year)%>%
    summarise(
      "Classified sightings"=sum(`Total sightings`,na.rm=TRUE),
      "Unique sightings"=n(),
      "Sightings per shark"=round(mean(`Total sightings`,na.rm=TRUE),2),
      "Scar %"=round(100*mean(`Identified scars`=="yes"),2),
      "Average size"=round(mean( `Size (mean)`,na.rm=TRUE),2),
      "Male/Female ratio"=round(sum(`Sex (mode)`=="male",na.rm=TRUE)/
                                  sum(`Sex (mode)`=="female",na.rm=TRUE),2),
      "Tag %"=round(100*mean(`Tag count`,na.rm=TRUE),2),
      "Drone measurement %"=round(100*mean(`Drone measurements`,na.rm=TRUE),2),
      "Prey sample %"=round(100*mean(`Prey samples`,na.rm=TRUE),2))}


get_summary_stats_weekly<-function(df){
  summary_stats_weekly<-df%>%
    group_by(`Week start`)%>%
    summarise(
      "Classified sightings"=sum(`Total weekly sightings`,na.rm=TRUE),
      "Unique sightings"=n(),
      "Sightings per shark"=round(mean(`Total weekly sightings`,na.rm=TRUE),2),
      "Scar %"=round(100*mean(`Identified scars`=="yes",na.rm=TRUE),2),
      "Average size"=round(mean( `Size (mean)`,na.rm=TRUE),2),
      "Male/Female ratio"=round(sum(`Sex (mode)`=="male",na.rm=TRUE)/
                                  sum(`Sex (mode)`=="female",na.rm=TRUE),2),
      "Tag %"=round(100*mean(`Tag count`,na.rm=TRUE),2),
      "Drone measurement %"=mean(`Drone measurents`,na.rm=TRUE),
      "Prey sample %"=round(100*mean(`Prey samples`,na.rm=TRUE),2))%>%
    arrange(desc(`Week start`))}



delete_sighting <- function(id_for_deletion){
  mapping_old<-s3readRDS(object = "map.rds", bucket = "mada-whales")
  
  map<-mapping_old%>%
    filter(!sighting_id %in% id_for_deletion)
  
  s3saveRDS(x = map, bucket = "mada-whales", object = "map.rds")  
}
