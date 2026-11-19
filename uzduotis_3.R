library(sf)
library(plotly)
library(readxl)
library(dplyr)
library(shiny)
library(DT)

map <- read_sf('admin_ribos.shp')
map <- st_transform(map, crs = 4326)
sav_map <- map[map$ADMIN_LEVE==5,]
sav_map <- st_cast(sav_map, "POLYGON")
sav_map <- sav_map %>% arrange(NAME)
sav_map <- st_simplify(sav_map, dTolerance = 500, preserveTopology = TRUE)

matematika <- read_excel(path="matematikos_olimpiados_laimėtojai_2026.xlsx")
ekonomika <- read_excel(path="ekonomikos_olimpiados_laimėtojai_2026.xlsx")
chemija <- read_excel(path="chemijos_olimpiados_laimėtojai_2026.xlsx")

grupavimas1 <- matematika %>%
  group_by(savivaldybe) %>%
  tally()

grupavimas2 <- ekonomika %>%
  group_by(savivaldybe) %>%
  tally()

grupavimas3 <- chemija %>%
  group_by(savivaldybe) %>%
  tally()

grupavimas1 <- grupavimas1 %>% arrange(savivaldybe)
grupavimas2 <- grupavimas2 %>% arrange(savivaldybe)
grupavimas3 <- grupavimas3 %>% arrange(savivaldybe)

savivaldybes <- data.frame(
  savivaldybe = c(
    "Akmenės r. sav.",
    "Alytaus m. sav.",
    "Alytaus r. sav.",
    "Anykščių r. sav.",
    "Birštono sav.",
    "Biržų r. sav.",
    "Druskininkų sav.",
    "Elektrėnų sav.",
    "Ignalinos r. sav.",
    "Jonavos r. sav.",
    "Joniškio r. sav.",
    "Jurbarko r. sav.",
    "Kaišiadorių r. sav.",
    "Kalvarijos sav.",
    "Kauno m. sav.",
    "Kauno r. sav.",
    "Kazlų Rūdos sav.",
    "Kelmės r. sav.",
    "Klaipėdos m. sav.",
    "Klaipėdos r. sav.",
    "Kretingos r. sav.",
    "Kupiškio r. sav.",
    "Kėdainių r. sav.",
    "Lazdijų r. sav.",
    "Marijampolės sav.",
    "Mažeikių r. sav.",
    "Molėtų r. sav.",
    "Neringos sav.",
    "Pagėgių sav.",
    "Pakruojo r. sav.",
    "Palangos m. sav.",
    "Panevėžio m. sav.",
    "Panevėžio r. sav.",
    "Pasvalio r. sav.",
    "Plungės r. sav.",
    "Prienų r. sav.",
    "Radviliškio r. sav.",
    "Raseinių r. sav.",
    "Rietavo sav.",
    "Rokiškio r. sav.",
    "Skuodo r. sav.",
    "Šakių r. sav.",
    "Šalčininkų r. sav.",
    "Šiaulių m. sav.",
    "Šiaulių r. sav.",
    "Šilalės r. sav.",
    "Šilutės r. sav.",
    "Širvintų r. sav.",
    "Švenčionių r. sav.",
    "Tauragės r. sav.",
    "Telšių r. sav.",
    "Trakų r. sav.",
    "Ukmergės r. sav.",
    "Utenos r. sav.",
    "Varėnos r. sav.",
    "Vilkaviškio r. sav.",
    "Vilniaus m. sav.",
    "Vilniaus r. sav.",
    "Visagino sav.",
    "Zarasų r. sav."
  )
)

bendra <- left_join(savivaldybes,grupavimas1,by='savivaldybe')
bendra <- left_join(bendra,grupavimas2,by='savivaldybe')
bendra <- left_join(bendra,grupavimas3,by='savivaldybe')
bendra <- bendra %>% arrange(savivaldybe)

bendra$n[is.na(bendra$n)] <- 0
bendra$n.x[is.na(bendra$n.x)] <- 0
bendra$n.y[is.na(bendra$n.y)] <- 0

bendra$Skaicius <- bendra$n + bendra$n.x + bendra$n.y

sav_map$Skaicius <- bendra$Skaicius

sav_map$Pav <- bendra$savivaldybe

zemelapis <- plot_ly(sav_map,
                     customdata = ~NAME,
                     split = ~NAME,           
                     color = ~log(Skaicius+1),
                     colors = colorRamp(c("gray", "orange", "red", "darkred")),      
                     text = ~paste0("<b>", NAME, "</b><br>Laimėtojai: ", Skaicius),
                     hoverinfo = "text",
                     hoveron = "fills",       
                     showlegend = FALSE,
                     stroke = I("white"),
                     span = I(0.5)) |>
  colorbar(title = "Laimėtojai",tickvals = log(c(1, 2, 5, 10, 20, 58) + 1),ticktext = c("1", "2", "5", "10", "20", "58"))


visi <- bind_rows(
  matematika %>% mutate(olimpiada = "Matematika"),
  ekonomika  %>% mutate(olimpiada = "Ekonomika"),
  chemija    %>% mutate(olimpiada = "Chemija"))

skaiciuoti_zemelapi <- function(duomenys) {
  grupuota <- duomenys %>% group_by(savivaldybe) %>% tally()
  bendra_lok <- left_join(savivaldybes, grupuota, by = "savivaldybe") %>% arrange(savivaldybe)
  bendra_lok$n[is.na(bendra_lok$n)] <- 0
  bendra_lok$n
}

apdovanojimai_pagal_olimpiada <- function(pasirinktos) {
  turi_chemija <- "Chemija" %in% pasirinktos
  turi_mat_ek  <- any(c("Matematika", "Ekonomika") %in% pasirinktos)
  if (turi_chemija && turi_mat_ek) {
    c("I laipsnio diplomas", "II laipsnio diplomas", "III laipsnio diplomas",
      "I vieta", "II vieta", "III vieta", "Pagyrimo rastas")
  } else if (turi_chemija) {
    c("I vieta", "II vieta", "III vieta", "Pagyrimo rastas")
  } else {
    c("I laipsnio diplomas", "II laipsnio diplomas", "III laipsnio diplomas", "Pagyrimo rastas")
  }
}

ui <- fluidPage(
  fluidRow(
    column(12,
           radioButtons("rodymas", "Rodyti:",
                        choices = c("Žemėlapis ir lentelė" = "abu",
                                    "Tik žemėlapis"        = "tik_zemelapis",
                                    "Tik lentelė"          = "tik_lentele"),
                        selected = "abu", inline = TRUE)
    )
  ),
  fluidRow(
    column(4,
           checkboxGroupInput("olimpiados_pasirinkimas",
                              label    = "Olimpiada:",
                              choices  = c("Matematika", "Ekonomika", "Chemija"),
                              selected = c("Matematika", "Ekonomika", "Chemija"),
                              inline   = TRUE)
    ),
    column(8,
           checkboxGroupInput("vieta_filter",
                              label    = "Apdovanojimas:",
                              choices  = c("I laipsnio diplomas", "II laipsnio diplomas", "III laipsnio diplomas",
                                           "I vieta", "II vieta", "III vieta", "Pagyrimo rastas"),
                              selected = c("I laipsnio diplomas", "II laipsnio diplomas", "III laipsnio diplomas",
                                           "I vieta", "II vieta", "III vieta", "Pagyrimo rastas"),
                              inline   = TRUE)
    )
  ),
  fluidRow(
    conditionalPanel(
      condition = "input.rodymas == 'abu' || input.rodymas == 'tik_zemelapis'",
      column(7, plotlyOutput("zemelapis", height = "600px"))
    ),
    conditionalPanel(
      condition = "input.rodymas == 'abu' || input.rodymas == 'tik_lentele'",
      column(5,
             uiOutput("antraste"),
             DTOutput("lentele"))
    )
  )
)

server <- function(input, output, session) {
  
  observeEvent(input$olimpiados_pasirinkimas, ignoreNULL = FALSE, {
    nauji <- apdovanojimai_pagal_olimpiada(input$olimpiados_pasirinkimas)
    updateCheckboxGroupInput(session, "vieta_filter",
                             choices  = nauji,
                             selected = nauji)
  })
  
  filtruoti <- reactive({
    d <- visi
    if (length(input$olimpiados_pasirinkimas) > 0)
      d <- d %>% filter(olimpiada %in% input$olimpiados_pasirinkimas)
    if (length(input$vieta_filter) > 0)
      d <- d %>% filter(apdovanojimas %in% input$vieta_filter)
    d
  })
  
  output$zemelapis <- renderPlotly({
    zm <- sav_map
    zm$Skaicius <- skaiciuoti_zemelapi(filtruoti())
    zm$Pav      <- bendra$savivaldybe
    
    maks <- max(zm$Skaicius, na.rm = TRUE)
    tick_vals_raw <- unique(c(c(1, 2, 5, 10, 20, 50)[c(1, 2, 5, 10, 20, 50) <= max(maks, 1)], maks))
    
    plot_ly(zm,
            split      = ~NAME,
            customdata = ~Pav,
            color      = ~log(Skaicius + 1),
            colors     = colorRamp(c("gray", "orange", "red", "darkred")),
            text       = ~paste0("<b>", NAME, "</b><br>Laimėtojai: ", Skaicius),
            hoverinfo  = "text",
            hoveron    = "fills",
            showlegend = FALSE,
            stroke     = I("white"),
            span       = I(0.5),
            source     = "z"
    ) |>
      colorbar(title = "Laimėtojai",
               tickvals = log(tick_vals_raw + 1),
               ticktext = as.character(tick_vals_raw)) |>
      event_register("plotly_click")
  })
  
  paspausta <- reactive({
    klik <- event_data("plotly_click", source = "z")
    req(klik)
    bendra$savivaldybe[klik$curveNumber + 1]
  })
  
  output$antraste <- renderUI({
    req(paspausta())
    h3(paste(paspausta()))
  })
  
  output$lentele <- renderDT({
    req(paspausta())
    duomenys <- filtruoti() %>% filter(savivaldybe == paspausta())
    duomenys %>%
      select(Olimpiada = olimpiada, "Vardas Pavardė" = vardas_pavarde,
             Mokykla = mokykla, Klasė = klase, Apdovanojimas = apdovanojimas) %>%
      datatable(rownames = FALSE,
                options = list(scrollX = TRUE, scrollY = "300px", paging = FALSE))
  })
}

shinyApp(ui, server)



