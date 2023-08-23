url_client<-"https://kf.kobotoolbox.org/api/v2/assets/aRiFqCYpV9hzifBCTDszcR/data.json"

u<-"madawhale"
pw<-"Wh6l3Sh6rk"

# Call form_meta API and Parse JSON
rawdata_client<-GET(url_client,authenticate(u,pw),progress())
p<-jsonlite::parse_json(rawdata_client)
result_client<-p$results

df_client<-tibble(list_col=result_client)%>%
  unnest_wider(list_col)%>%
  select(first_name,surname,email,support_method,newsletter,photos)
