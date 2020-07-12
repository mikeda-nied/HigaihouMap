library(dplyr)
library(RCurl)
library(jsonlite)
library(sf)
library(jpndistrict)

city_union<-function(x) {
  try(function(x){
    x <- x %>% sf::st_make_valid()
  },silent=T)
  x %>% 
    sf::st_cast("MULTIPOLYGON") %>% 
    sf::st_union(by_feature = FALSE) %>% 
    sf::st_transform(crs = 4326) %>% 
    sf::st_cast("POLYGON") %>% 
    purrr::map(~.x[1]) %>% 
    sf::st_multipolygon() %>% 
    sf::st_sfc(crs = 4326)
}

jpn_cities2<-function(jis_code, admin_name=NULL){
  jis_code<-as.character(jis_code)
  if(is.null(admin_name)){cities<-jpn_cities(jis_code)
  }else{cities<-jpn_cities(jis_code,admin_name)}
  api<-readLines('./RESAS_API')
  api.key<-'smHq5kzLohJ2FS75AI4ga5qeS8S0E0RG6eE3yqfK'
  json<-fromJSON(getURL(api,httpheader=paste('X-API-KEY:',api.key)))$result
  bc<-filter(json,bigCityFlag==2)
  if(2%in%lapply(jis_code,nchar)){
    jis_code<-lapply(jis_code,substr,1,2)%>%unlist()
    codes<-lapply(jis_code,function(c)grep(sprintf('^%s.*',c),bc$cityCode,value=T))%>%unlist()
  }else{
    codes<-intersect(jis_code,bc$cityCode)
  }
  if(length(codes)>0){
    bc<-filter(json,bigCityFlag==1)
    cities<-filter(cities,!city_code%in%bc$cityCode)
    cities<-lapply(codes,function(code){
      name<-json[json$cityCode==code,"cityName"]
      geom<-grep(name,bc$cityName)%>%bc[.,2]%>%jpn_cities()%>%st_geometry()%>%city_union()
      return(st_sf(city_code=code,city=name,geometry=geom))
    })%>%mapedit:::combine_list_of_sf()%>%rbind(cities)
  }
  return(cities)
}
