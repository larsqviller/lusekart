#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


#rsconnect::setAccountInfo(name='larsqviller',
#token='D1145CB7E92797F291018CEDC5CBFDD3',
#secret='H7d8s3dcSLDq649O0Znp2ZGAlaiDEqi4ykHVdjuV')

#rsconnect::deployApp('/home/lars/Dropbox/Vetinst_min_egen/shiny/smitteApp/kartapp/app.R')



## rm(list=ls(all=T))

## Setter stier
#sti <- '/home/lars/Dropbox/Vetinst_min_egen/shiny/smitteApp/kartapp/publish/'
#sti <- 'E:/Dropbox/Vetinst_min_egen/shiny/smitteApp/kartapp/data/'
#sti <- 'http://odin.vetinst.no/ta/pd/smittepress/publish/'

library('leaflet')
library('raster')
library('gdistance')
library('rgdal')
library('rgeos')
library('sp')
library('shiny')
library('rsconnect')


# Funksjoner --------------------------------------------------------------
## Velger lokaliteter innenfor omkretsen som settes i kartet
velgelok <- function(omkr, lokvalg, kartvalg){
  omkrets   <- omkr* 1000
  aktiv.lok <- velgAktiv(kartvalg)[[1]]
  lok.valg  <- lok[lok$LOK_NR == lokvalg,]
  lok.valg.trans <- spTransform(lok.valg, CRS("+init=epsg:32633"))
  aktiv.lok.trans <- spTransform(aktiv.lok, CRS("+init=epsg:32633"))
  lok.omkrets <- gBuffer(lok.valg.trans,width=omkrets)
  lok.omkrets.trans <- spTransform(lok.omkrets, CRS("+init=epsg:4326"))
  over_lok <- as.logical(over(aktiv.lok.trans, lok.omkrets, returnList = T))
  over_lok[is.na(over_lok)] <- F
  lok.clip    <- aktiv.lok[over_lok,]
  data_ut <- list(lok.clip, lok.omkrets.trans,lok.omkrets)
  return(data_ut)
}

velgTilListe <- function(lokliste, valgtliste){
  redusertListe <- lokliste[lokliste$LOKALITETSNUMMER %in% valgtliste$LOK_NR, ]
  redusertListe.sort <- redusertListe[order(redusertListe$LOKALITETSNUMMER),] 
  return(redusertListe.sort)
}
## Velger ut rasterkartet som gjelder for uken
velgRaster <- function(valgtuke){
  smittekart <- subset(pressene, which(names(pressene) == valgtuke))
  return(smittekart)
}

velgAktiv <- function(valgtuke){
  ukenummer  <- substring(valgtuke, 7,nchar(valgtuke))
  aktiv_now <- aktive[which(substring(names(aktive),2,nchar(names(aktive))) == ukenummer)]
  aktiv.lok <-  lok[lok$LOK_NR %in% rownames(na.omit(aktiv_now)),]
  aktivdata <- list(aktiv.lok, aktiv_now)
  return(aktivdata)
}


redLeafIcon <- makeIcon(
  iconUrl = "map_marker_red.png",
  iconWidth = 26, iconHeight = 40,
  iconAnchorX = 13, iconAnchorY = 40
)

dataliste <- function(kartvalg){
  aktiv.lok <- velgAktiv(kartvalg)[[1]]
  aktiv.pos = which(names(aktive) %in% names(velgAktiv(kartvalg)[[2]]))
  
  Totale_sisteuke <-  data.frame("Totalt smittepress mobile" = log(Totalt[, aktiv.pos]+1), LOK_NR = as.numeric(substring(row.names(Totalt), 1,7)))
  #Totale_ad_sisteuke <-  data.frame("Totalt smittepress voksne" = log(Totalt.ad[, aktiv.pos]+1), LOK_NR = as.numeric(substring(row.names(Totalt.ad), 1,7)))
  
  tamed <- c("TILL_KOM", "FORMÅL", "PRODUKSJONSFORM", "LOK_NR", "LOK_NAVN", "N_GEOWGS84", "Ø_GEOWGS84")
  
  lokreg_redus <- lokreg[lokreg$LOK_NR %in% aktiv.lok$LOK_NR,]
  lokreg_redus <- lokreg_redus[!duplicated(lokreg_redus$LOK_NR),]
  lokreg_redus <- lokreg_redus[,names(lokreg_redus) %in% tamed]
  dim(lokreg_redus)
  
  lokreg_redus <- merge(lokreg_redus, Totale_sisteuke, by = "LOK_NR")
  #lokreg_redus <- merge(lokreg_redus, Totale_ad_sisteuke, by = "LOK_NR")
  #lokreg_redus <- merge(lokreg_redus, interne_sisteuke, by = "LOK_NR")
  dim(lokreg_redus)
  head(lokreg_redus)
  
  names(lokreg_redus)[c(1,2, 5)] <- c("LOKALITETSNUMMER", "KOMMUNE", "LOKALITETSNAVN")
  return(lokreg_redus)
}


lesekart <- function(mappe){
  dir(mappe)
}

# Laster data -------------------------------------------------------------

## Laster Lokreg-shape
lok            <- readOGR('./lokreg_geo.shp', layer = "lokreg_geo")
aktive_alle    <- read.table('./Aktiv.txt', header=T, sep = '\t', dec = ',')
aktive_alle[row.names(aktive_alle) == '10173',] <- 1
aktive0        <- aktive_alle[,c(521: ncol(aktive_alle))]
aktive0[,ncol(aktive0)] <- aktive0[,ncol(aktive0)-1]
aktive         <- cbind(aktive0, XNextWeek1 = aktive0[,ncol(aktive0)-1], XNextWeek2 = aktive0[,ncol(aktive0)-1], XNextWeek3 = aktive0[,ncol(aktive0)-1])
#ncol(aktive)
#aktiv_now      <- row.names(aktive)[!is.na(aktive[,ncol(aktive)-3])]
#aktiv.lok      <-  lok[lok$LOK_NR %in% aktiv_now,]

#lokreg_redus <- read.table('./lokreg_redus.csv', sep = ';', dec =",")

press201621 <- raster('./rasters/pressX201621.tif')
press201622 <- raster('./rasters/pressX201622.tif')
press201623 <- raster('./rasters/pressX201623.tif')
press201624 <- raster('./rasters/pressX201624.tif')
press201625 <- raster('./rasters/pressX201625.tif')
press201626 <- raster('./rasters/pressX201626.tif')
press201627 <- raster('./rasters/pressX201627.tif')
press201628 <- raster('./rasters/pressX201628.tif')
press201629 <- raster('./rasters/pressX201629.tif')
press201630 <- raster('./rasters/pressX201630.tif')
press201631 <- raster('./rasters/pressX201631.tif')
press201632 <- raster('./rasters/pressX201632.tif')
press201633 <- raster('./rasters/pressX201633.tif')
press201634 <- raster('./rasters/pressX201634.tif')
press201635 <- raster('./rasters/pressX201635.tif')
press201636 <- raster('./rasters/pressX201636.tif')
press201637 <- raster('./rasters/pressX201637.tif')
press201638 <- raster('./rasters/pressX201638.tif')
press201639 <- raster('./rasters/pressX201639.tif')
press201640 <- raster('./rasters/pressX201640.tif')
press201641 <- raster('./rasters/pressX201641.tif')
press201642 <- raster('./rasters/pressX201642.tif')
press201643 <- raster('./rasters/pressX201643.tif')
press201644 <- raster('./rasters/pressX201644.tif')
press201645 <- raster('./rasters/pressX201645.tif')
press201646 <- raster('./rasters/pressX201646.tif')
press201647 <- raster('./rasters/pressX201647.tif')
press201648 <- raster('./rasters/pressX201648.tif')
press201649 <- raster('./rasters/pressX201649.tif')
press201650 <- raster('./rasters/pressX201650.tif')
press201651 <- raster('./rasters/pressX201651.tif')
press201652 <- raster('./rasters/pressX201652.tif')
press201701 <- raster('./rasters/pressX201701.tif')
press201702 <- raster('./rasters/pressX201702.tif')
press201703 <- raster('./rasters/pressX201703.tif')
press201704 <- raster('./rasters/pressX201704.tif')
press201705 <- raster('./rasters/pressX201705.tif')
press201706 <- raster('./rasters/pressX201706.tif')
press201707 <- raster('./rasters/pressX201707.tif')
press201708 <- raster('./rasters/pressX201708.tif')
press201709 <- raster('./rasters/pressX201709.tif')
press201710 <- raster('./rasters/pressX201710.tif')
pressNextWeek1 <- raster('./rasters/pressXNextWeek1.tif')
pressNextWeek2 <- raster('./rasters/pressXNextWeek2.tif')
#pressNeste3 <- raster('./rasters/pressNeste3.tif')

extent(press201705) <- extent(press201704)

pressene <- stack(press201621, press201622, press201623, press201624, press201625, press201626, press201627, press201628, press201629, press201630, press201631, press201632, press201633, press201634, press201635, press201636, press201637, press201638, press201639, press201640, press201641, press201642, press201643,press201644, press201645,press201646,press201647,press201648,press201649,press201650,press201651,press201652,press201701, press201702, press201703, press201704, press201705, press201706,press201707,press201708,press201709,press201710,
                  pressNextWeek1, pressNextWeek2)

#names(pressene)[9] <- 'pressXNextWeek1'
#names(pressene)[10] <- 'pressXNextWeek2'
names(pressene)
fargegrunnlag <- raster('./fargegrunnlag100.tif')




## Leser inn lusedata
#mobile <- read.table('./BevegligeLus.txt', header=T, sep = '\t', dec = ',')
lokreg <- read.table('./Locreg2016-10-17x.txt', header=T, sep = '\t', dec = ',', fileEncoding = "cp1252")
Totalt <- read.table('./MobileTotaltFra2012.txt', header=T, sep = '\t', dec = ',')
#Totalt.ad <- read.table('./HunnLusTotaltFra2012.txt', header=T, sep = '\t', dec = ',')
#internt <- read.table(paste0(sti, 'prodMobileDagFra2012.txt'), header=T, sep = '\t', dec = ',')


#dataliste(input$kartvalg)

## Henter ut siste smittepress og lusetall:




#pal <- colorQuantile(c("#ffffff","#ffffcc","#ffeda0","#fed976","#feb24c","#fd8d3c","#fc4e2a","#e31a1c", "#bd0026", "#800026"), probs = seq(0,1,0.1), values(fargegrunnlag), na.color = "transparent")
pal <- colorQuantile(c("#ffffff","#ffffff","#ffffcc","#ffffcc","#fed976","#fed976","#feb24c","#e31a1c", "#bd0026", "#800026"), probs = seq(0,1,0.1), values(fargegrunnlag), na.color = "transparent")


ui <- fluidPage(
  navbarPage("Veterinærinstituttets smittepress -Betaversjon", id="nav",
             
             tabPanel("Interaktivt kart",
                      div(class="outer",
                          tags$head(
                            # Include Shiny's our custom CSS
                            includeCSS("./styles.css"),
                            includeScript("./gomap.js")
                          ),
                          leafletOutput("mymap", width = "100%", height="100%"),
                          absolutePanel(id = "controls", 
                                        class = "panel panel-default", 
                                        fixed = TRUE,
                                        draggable = TRUE, 
                                        top = 135, 
                                        left = 20, 
                                        right = "auto", 
                                        bottom = "auto",
                                        width = 330, 
                                        height = "auto", 
                                        h2("Tilpass visning"),
                                        
                                        textInput("lok", "Velg lokalitet", 
                                                  #value = aktiv.lok$LOK_NR[order(aktiv.lok$LOK_NR)[1]]
                                                  value = "10173"),
                                        verbatimTextOutput("value"),
                                        sliderInput("km_om",
                                                    "Kilometer radius:",
                                                    min = 1,
                                                    max = 100,
                                                    value = 20),
                                        checkboxGroupInput("visvar", "Vis",
                                                           c("Vis smittekart over hele Norge" = "smi",
                                                             "Vis bare smittekart" = "lokmark",
                                                             "Vis alle aktive lokaliteter i Norge" = "begr")),
                                        selectInput('kartvalg', "Velg uke:",
                                                    c('Uke 201710' = 'pressX201710', 
                                                      'Uke 201621' = 'pressX201621',
                                                      'Uke 201622' = 'pressX201622',
                                                      'Uke 201623' = 'pressX201623', 
                                                      'Uke 201624' = 'pressX201624',
                                                      'Uke 201625' = 'pressX201625',
                                                      'Uke 201626' = 'pressX201626',
                                                      'Uke 201627' = 'pressX201627',
                                                      'Uke 201628' = 'pressX201628',
                                                      'Uke 201629' = 'pressX201629',
                                                      'Uke 201630' = 'pressX201630',
                                                      'Uke 201631' = 'pressX201631',
                                                      'Uke 201632' = 'pressX201632',
                                                      'Uke 201633' = 'pressX201633',
                                                      'Uke 201634' = 'pressX201634',
                                                      'Uke 201635' = 'pressX201635',
                                                      'Uke 201636' = 'pressX201636',
                                                      'Uke 201637' = 'pressX201637',
                                                      'Uke 201638' = 'pressX201638',
                                                      'Uke 201639' = 'pressX201639',
                                                      'Uke 201640' = 'pressX201640',
                                                      'Uke 201641' = 'pressX201641',
                                                      'Uke 201642' = 'pressX201642',
                                                      'Uke 201643' = 'pressX201643', 
                                                      'Uke 201644' = 'pressX201644', 
                                                      'Uke 201645' = 'pressX201645', 
                                                      'Uke 201646' = 'pressX201646', 
                                                      'Uke 201647' = 'pressX201647', 
                                                      'Uke 201648' = 'pressX201648', 
                                                      'Uke 201649' = 'pressX201649', 
                                                      'Uke 201650' = 'pressX201650', 
                                                      'Uke 201651' = 'pressX201651', 
                                                      'Uke 201652' = 'pressX201652', 
                                                      'Uke 201701' = 'pressX201701',
                                                      'Uke 201702' = 'pressX201702',
                                                      'Uke 201703' = 'pressX201703',
                                                      'Uke 201704' = 'pressX201704',
                                                      'Uke 201705' = 'pressX201705',
                                                      'Uke 201706' = 'pressX201706',
                                                      'Uke 201707' = 'pressX201707',
                                                      'Uke 201708' = 'pressX201708',
                                                      'Uke 201709' = 'pressX201709', 
                                                      'En uke fram i tid' = 'pressXNextWeek1',
                                                      'To uker fram i tid' = 'pressXNextWeek2'
                                                      ))
                          ) 
                      )
             ),
             
             tabPanel("Lokalitetsliste",
                      radioButtons("listevalg", "Velg liste:",
                                   c("Valgte lokaliteter" = "valgt",
                                     "Alle aktive lokaliteter" = "hele"
                                     )),
                      tableOutput("mytable")
             ),
             tabPanel("Dokumentasjon",
                       includeMarkdown("Dokumentasjon.Rmd")
                      
             ),
             tabPanel("Kildekode",
                      includeMarkdown("script.Rmd")
             )
              
  )
)





server <- function(input, output, session) {
  output$mymap <- renderLeaflet({
    {if(!("lokmark" %in% input$visvar)) {
      
      
      leaflet({if(!("begr" %in% input$visvar)) data = velgelok(input$km_om, input$lok, input$kartvalg)[[1]] else data = velgAktiv(input$kartvalg)[[1]]}) %>% 
        addTiles()%>% 
        addMarkers(popup = ~as.character(LOK_NR)) %>%
        addMarkers(data = lok[lok$LOK_NR == input$lok,],popup = ~as.character(LOK_NR), icon = redLeafIcon) %>%
        addRasterImage({if(!("smi" %in% input$visvar)) crop(velgRaster(input$kartvalg), 
                                                            velgelok(input$km_om, 
                                                                     input$lok, input$kartvalg)[[3]]) else velgRaster(input$kartvalg)}, 
                       maxBytes=Inf, 
                       colors = pal, 
                       opacity = 0.65)  %>% 
        addLegend(pal = pal, values = values(fargegrunnlag), title = "Smittepress") %>% 
        addPolygons(data=velgelok(input$km_om, input$lok, input$kartvalg)[[2]], weight = , fillColor = "transparent") 
    }
      else
        leaflet({if(!("begr" %in% input$visvar)) data = velgelok(input$km_om, input$lok, input$kartvalg)[[1]] else data = velgAktiv(input$kartvalg)[[1]]}) %>% 
        addTiles()%>% 
        addRasterImage({if(!("smi" %in% input$visvar)) crop(velgRaster(input$kartvalg), 
                                                            velgelok(input$km_om, 
                                                                     input$lok, input$kartvalg)[[3]]) else velgRaster(input$kartvalg)}, 
                       maxBytes=Inf, 
                       colors = pal, 
                       opacity = 0.35)  %>% 
        addLegend(pal = pal, values = values(fargegrunnlag), title = "Smittepress") %>% 
        addPolygons(data=velgelok(input$km_om, input$lok, input$kartvalg)[[2]], weight = 3, fillColor = "transparent") 
    }
  })
  
    output$mytable <- renderTable({if(input$listevalg == "valgt") 
    velgTilListe(dataliste(input$kartvalg),velgelok(input$km_om, input$lok, input$kartvalg)[[1]])
    else
      dataliste(input$kartvalg)[order(dataliste(input$kartvalg)$LOKALITETSNUMMER),]
    
  })

  
}

shinyApp(ui, server)
