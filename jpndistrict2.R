targetPackages <- c('dplyr', 'RCurl', 'jsonlite', 'sf', 'devtools') 
newPackages <- targetPackages[!(targetPackages %in% installed.packages()[,"Package"])]
if(length(newPackages)) install.packages(newPackages, repos = "https://cran.ism.ac.jp/")
for(package in targetPackages) library(package, character.only = T)
if(!"jpndistrict" %in% installed.packages()[,"Package"]) devtools::install_github("uribo/jpndistrict")
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
  cd<-as.character(jis_code)
  cd[cd=="40231"]<-"40305" #2018.10.01の市町村変更に対応
  if(is.null(admin_name)){cities<-jpn_cities(cd)
  }else{cities<-jpn_cities(cd,admin_name)}
  cities<-as.data.frame(cities)
  if("40231"%in%jis_code){
    cities[cities$city_code=="40305",]$city_code<-"40231" #2018.10.01の市町村変更に対応
    cities[cities$city_code=="40231",]$city<-"那珂川市"   #2018.10.01の市町村変更に対応
  }
  if("28221"%in%jis_code){
    cities[cities$city_code=="28221",]$city<-"丹波篠山市" #2019.05.01の市町村変更に対応
  }
  cities<-cities%>%as_tibble()%>%sf::st_as_sf()
  api<-"https://opendata.resas-portal.go.jp/api/v1/cities"
  api.key<-readLines("./RESAS_API")
  json<-fromJSON(getURL(api,httpheader=paste('X-API-KEY:',api.key)))$result
  bc<-filter(json,bigCityFlag==2)
  if(2%in%lapply(cd,nchar)){
    cd<-lapply(cd,substr,1,2)%>%unlist()
    codes<-lapply(cd,function(c)grep(sprintf('^%s.*',c),bc$cityCode,value=T))%>%unlist()
  }else{
    codes<-intersect(cd,bc$cityCode)
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
