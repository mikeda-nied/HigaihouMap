library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinyWidgets)
library(dplyr)
library(jsonlite)
library(jpndistrict)
library(DT)
library(sf)
library(geojsonsf)
library(leaflet)
library(mapview)
source('jpndistrict2.R')

# 設定
appURL <- 'https://script.google.com/macros/s/AKfycbw_IoRJvy3nLLKkgzgcsVQ9nsCFAT23lLVuIQtRw1xaV1Fzc4A/exec'
sheetInfo <- rbind('20'=c('長野県','1WQb7azSvJq86QVr8OSDVcmL7ZyhoTRbChBhF2WimaqE'),
                   '21'=c('岐阜県','14qgYilvobPdCZJ7VCUVwsV-5tPGk1YCj5JqnEkPgkOw'),
                   '40'=c('福岡県','1TD7fQvL1UreUpCODjR2pnv8UouV6ICiGqKDftikVtRQ'),
                   '42'=c('長崎県','1dUba9iSUdmk4Pzfuzh9JCCwv_FJnM1b5sN-PkzTy3EA'),
                   '43'=c('熊本県','19vj2RkcId6B8gLIanlNyna-zMwM8aL3W_qQwXd5Hm2Y'),
                   '44'=c('大分県','1V3Q8-KnYBQuG8yiiuvcWDavuTCGbMjw3_-bzmXZPwZw'),
                   '45'=c('宮崎県','1nR7WOwh0hvAUYJaR-gPOlyXQIgUus6zKbGeTIBEiUqU'),
                   '46'=c('鹿児島県','1afPdtQagE00CW3qcwxUEGWNTEHheCRjmjci_kCzETIQ'))
colnames(sheetInfo) <- c('prefName', 'sid')
init <- '40'
optSheet <- NULL
optPref <- rownames(sheetInfo)
names(optPref) <- sheetInfo[,'prefName']
optPref <- c(optPref, 'まとめて'='all')
options(spinner.color="#605CA8", spinner.color.background="#ffffff", spinner.size=2)
spins <- c('circle','bounce','folding-cube','rotating-plane','cube-grid','fading-circle','double-bounce','dots','cube')

dansui_sid <- '1_D874Mftuhs-DC_FTiIfX8aOuWJJJdifh6-yggrXxnY'
dansui_optSheet <- fromJSON(sprintf('%s?action=getsheetname&id=%s', appURL, dansui_sid)) %>% rev()

# 地図の設定
dmgPal <- colorRampPalette(c("transparent", "orange", "darkred", "black"), alpha=T)
dmdCol <- colorBin(palette=dmgPal(7), bins=c(Inf,10000,1000,100,10,1,0))


# UIヘッダ
header <- dashboardHeader(title=HTML('N<sup>2</sup>EM Scraper'), disable=FALSE, titleWidth=250)

# UIサイドバー
sidebar <- dashboardSidebar(
  width=250,
  sidebarMenu(
    id='menu',
    menuItem(
      '１．都道府県を選択',
      tabName='dataTable',
      startExpanded=T
    ),
    conditionalPanel(
      class="subMenus",
      condition="input.menu == 'dataTable'",
      radioButtons('cat', '情報種別：', c('住家被害'='housing', '断水'='water'), selected='housing')
    ),
    conditionalPanel(
      class="subMenus",
      condition="input.menu == 'dataTable' & input.cat == 'housing'",
      radioButtons('pref', '都道府県:', optPref, selected=init),
      addSpinner(htmlOutput('sheetInfo'), spin=spins[as.integer(runif(1,min=1,max=9))], color="#555299")
    ),
    conditionalPanel(
      class="subMenus",
      condition="input.menu == 'dataTable' & input.cat == 'water'",
      addSpinner(htmlOutput('dansui_sheetInfo'), spin=spins[as.integer(runif(1,min=1,max=9))], color="#555299")
    ),
    menuItem(
      '２．地図で確認',
      tabName='map'
    ),
    conditionalPanel(
      class="subMenus",
      condition="input.menu == 'map'",
      downloadButton('downloadJSON', 'GeoJSONをDownload')
    )
  )
)

# UIボディ
body <- dashboardBody(
  useShinyjs(),
  tabItems(
    tabItem(
      'dataTable',
      bootstrapPage(
        tags$style(type='text/css', '#table {min-height: calc(100vh - 80px) !important;}'),
        addSpinner(dataTableOutput('table'), spin=spins[as.integer(runif(1,min=1,max=9))], color="#555299")
      )
    ),
    tabItem(
      'map',
      bootstrapPage(
        tags$style(type='text/css', '#mapPlot {height: calc(100vh - 80px) !important;}'),
        addSpinner(leafletOutput('mapPlot'), spin=spins[as.integer(runif(1,min=1,max=9))], color="#555299")
      )
    )
  )
)

# UI
ui <- dashboardPage(header, sidebar, body, skin='purple',
                    tags$head(tags$link(rel="stylesheet", type="text/css", href="style.css")))

# Server
server <- function(input, output, session) {
  
  # 初期化
  shinyjs::disable("downloadJSON")
  stData <- NULL
  optSheet <- fromJSON(sprintf('%s?action=getsheetname&id=%s', appURL, sheetInfo[init,'sid'])) %>% rev()
  data <- jsonlite::fromJSON(sprintf('%s?action=getdata&id=%s&sheet=%s', appURL, sheetInfo[init,'sid'], optSheet[1]))
  lapply(5:10, function(i) {
    data[,i] <<- data[,i] %>% as.numeric()
  })
  data <- data[1:(nrow(data)-1),]
  data$自治体コード <- data$自治体コード %>% substr(1,5)
  output$table <- renderDataTable(data)
  output$dansui_sheetInfo <- renderUI(
    fluidPage(
      id='dansui_sheetInfo-input',
      textInput('dansui_sid', 'Sheet id:', value=dansui_sid),
      actionButton('s_dansui_sid', ''),
      selectInput('dansui_sname', 'Sheet name:', choices=dansui_optSheet),
      actionButton('s_dansui', 'データを読み込む')
    )
  )
  shinyjs::hideElement(selector='.loading-spinner')
  
  
  # 情報カテゴリを変更したら
  observeEvent(input$cat, {
    shinyjs::hideElement(selector='#table div')
  })
  
  # 都道府県を変更したら
  observeEvent(input$pref, {
    shinyjs::hideElement(selector='#sheetInfo-input')
    shinyjs::hideElement(selector='#table div')
    shinyjs::showElement(selector='.sidebar-menu .loading-spinner')
    optSheet<<-NULL
    if (input$pref=='all') {
      optSheet <<- lapply(sheetInfo[,"sid"], function(id) {
        fromJSON(sprintf('%s?action=getsheetname&id=%s', appURL, id)) %>% rev()
      })
      output$sheetInfo <- renderUI(
        fluidPage(
          id='sheetInfo-input',
          tags$p("上記全ての都道府県の最新のシートを読み込みます。"),
          tags$p("※結合時に人口カラムを除外しています。"),
          actionButton('s_pref', 'データを読み込む')
        )
      )
    } else {
      optSheet <<- fromJSON(sprintf('%s?action=getsheetname&id=%s', appURL, sheetInfo[input$pref,'sid'])) %>% rev()
      output$sheetInfo <- renderUI(
        fluidPage(
          id='sheetInfo-input',
          textInput('sid', 'Sheet id:', value=sheetInfo[input$pref, 'sid']),
          actionButton('s_sid', ''),
          selectInput('sname', 'Sheet name:', choices=optSheet),
          actionButton('s_pref', 'データを読み込む')
        )
      )
    }
    shinyjs::hideElement(selector='.sidebar-menu .loading-spinner')
  })
  
  # 都道府県を選択したら
  observeEvent(input$s_pref, {
    shinyjs::hideElement(selector='#table div')
    shinyjs::showElement(selector='.content .loading-spinner')
    
    if (input$pref=='all') {
      data <<- NULL
      sids <- sheetInfo[,'sid']
      sname <- names(sids) %>% lapply(function(p) optSheet[[p]][1]) %>% unlist()
      names(sname) <- names(sids)
      for (p in 1:length(sids)) {
        d <- jsonlite::fromJSON(sprintf('%s?action=getdata&id=%s&sheet=%s', appURL, sids[p], URLencode(sname[p])))
        lapply(5:10, function(i) {
          d[,i] <<- d[,i] %>% as.numeric()
        })
        d <- d[1:(nrow(d)-1), -4]
        data <<- rbind(data, d)
      }
    } else {
      sids <- c(input$sid)
      names(sids) <- input$pref
      sname <- c(input$sname)
      names(sname) <- input$pref
      data <- jsonlite::fromJSON(sprintf('%s?action=getdata&id=%s&sheet=%s', appURL, input$sid, input$sname))
      lapply(5:10, function(i) {
        data[,i] <<- data[,i] %>% as.numeric()
      })
      data <- data[1:(nrow(data)-1),]
    }
    data$自治体コード <- data$自治体コード %>% substr(1,5)
    output$table <- renderDataTable(data)
    shinyjs::showElement(selector='#table')
    shinyjs::hideElement(selector='.content .loading-spinner')
    shinyjs::disable("downloadJSON")
    data <<- data
    stData <<- NULL
  })
  
  # 断水データ読込
  observeEvent(input$s_dansui, {
    shinyjs::hideElement(selector='#table div')
    shinyjs::showElement(selector='.content .loading-spinner')
    data <- jsonlite::fromJSON(sprintf('%s?action=getdata&id=%s&sheet=%s', appURL, input$dansui_sid, input$dansui_sname))
    data <- jsonlite::fromJSON(sprintf('%s?action=getdata&id=%s&sheet=%s', appURL, dansui_sid, dansui_optSheet[1]))
    data$市町村コード <- data$市町村コード %>% substr(1,5)
    output$table <- renderDataTable(data)
    shinyjs::showElement(selector='#table')
    shinyjs::hideElement(selector='.content .loading-spinner')
    shinyjs::disable("downloadJSON")
    data <<- data
    stData <<- NULL
  })
  
  # シート再読込
  observeEvent(input$s_sid, {
    shinyjs::hideElement(selector='#sheetInfo-input')
    shinyjs::hideElement(selector='#table div')
    shinyjs::showElement(selector='.sidebar-menu .loading-spinner')
    optSheet<<-NULL
    optSheet <<- fromJSON(sprintf('%s?action=getsheetname&id=%s', appURL, input$sid)) %>% rev()
    output$sheetInfo <- renderUI(
      fluidPage(
        id='sheetInfo-input',
        textInput('sid', 'Sheet id:', value=input$sid),
        actionButton('s_sid', ''),
        selectInput('sname', 'Sheet name:', choices=optSheet),
        actionButton('s_pref', 'データを読み込む')
      )
    )
    shinyjs::hideElement(selector='.sidebar-menu .loading-spinner')    
  })
  observeEvent(input$s_dansui_sid, {
    shinyjs::hideElement(selector='#dansui_sheetInfo-input')
    shinyjs::hideElement(selector='#table div')
    shinyjs::showElement(selector='.sidebar-menu .loading-spinner')
    dansui_optSheet <<- fromJSON(sprintf('%s?action=getsheetname&id=%s', appURL, input$dansui_sid)) %>% rev()
    output$dansui_sheetInfo <- renderUI(
      fluidPage(
        id='dansui_sheetInfo-input',
        textInput('dansui_sid', 'Sheet id:', value=input$dansui_sid),
        actionButton('s_dansui_sid', ''),
        selectInput('dansui_sname', 'Sheet name:', choices=dansui_optSheet),
        actionButton('s_dansui', 'データを読み込む')
      )
    )
    shinyjs::hideElement(selector='.sidebar-menu .loading-spinner')    
  })
  
  
  observeEvent(input$menu, {
    # 地図を開いたら
    if (input$menu == 'map') {
      req(data)
      if (is.null(stData)) {
        
        shinyjs::hideElement(selector='#mapPlot')
        shinyjs::showElement(selector='.loading-spinner')
        
        # 住家被害の地図表示
        if (input$cat == 'housing') {
          stData <<- jpn_cities2(data$自治体コード) %>%
            dplyr::left_join(., data, by=c('city_code'='自治体コード'))
          output$mapPlot <- renderLeaflet({
            leaflet() %>%
              addTiles('https://cyberjapandata.gsi.go.jp/xyz/pale/{z}/{x}/{y}.png', group='背景地図',
                       attribution='<a href="https://maps.gsi.go.jp/development/ichiran.html" target="_blank">地理院タイル</a>') %>%
              addMapPane('overlay', zIndex=600) %>%
              addPolygons(data=stData, option=pathOptions(pane='overlay'), stroke=T, color='#666666', weight=0.5, 
                          fillColor=~dmdCol(合計), fillOpacity=0.8, popup=leafpop::popupTable(st_drop_geometry(stData[,colnames(stData)!="出典URL"]))) %>%
              addLegend(pal=dmdCol, values=c(Inf,0), opacity=0.8, title='住家被害 - 合計', position='topright') %>% 
              addScaleBar(position='bottomleft')
          })
        
        # 断水の地図表示
        } else if (input$cat == 'water') {
          stData <<- jpn_cities2(data$市町村コード) %>%
            dplyr::left_join(., data, by=c('city_code'='市町村コード'))
          output$mapPlot <- renderLeaflet({
            leaflet() %>%
              addTiles('https://cyberjapandata.gsi.go.jp/xyz/pale/{z}/{x}/{y}.png', group='背景地図',
                       attribution='<a href="https://maps.gsi.go.jp/development/ichiran.html" target="_blank">地理院タイル</a>') %>%
              addMapPane('overlay', zIndex=600) %>%
              addPolygons(data=stData, option=pathOptions(pane='overlay'), stroke=T, color='#666666', weight=0.5, 
                          fillColor=~dmdCol(戸数), fillOpacity=0.8,
                          popup=leafpop::popupTable(st_drop_geometry(stData))) %>%
              addLegend(pal=dmdCol, values=c(Inf,0), opacity=0.8, title='断水戸数', position='topright') %>% 
              addScaleBar(position='bottomleft')
          })
        }
        
        shinyjs::showElement(id='mapPlot')
        shinyjs::hideElement(selector='.loading-spinner')
        
        # GeoJSONを作成
        json <- sf_geojson(stData)
        output$downloadJSON = downloadHandler(
          filename = function() {
            sprintf("%s_%s.geojson", input$pref, input$sname)
          },
          content = function(f) {
            write(json, f)
          }
        )
        shinyjs::enable("downloadJSON")
      }
    }
  })
}

shinyApp(ui = ui, server = server)
