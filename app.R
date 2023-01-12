library(shiny)
library(shinythemes)
library(psych)
library(ggcorrplot)
library(ggplot2)
library(FSA)
library(psych)
library(skimr)
library(tidyverse)
library(countrycode)
library(corrplot)

# Data
bookings <- read.csv("data/hotel_bookings.csv",sep=",")

countres_list <- countrycode(unique(bookings$country), "iso3c", "country.name")

countres_list <- sort(countres_list)

int_objects_list <- colnames(Filter(is.numeric, bookings))

# UI
ui <- navbarPage(theme = shinytheme("cosmo"), "Hotellide broneerimine",
  tabPanel("Andmestik",
            sidebarLayout(
                sidebarPanel(
                    img(src = "logo.jpg", height = "100%", width = "100%"),
                    h4(strong("Rakenduse kirjeldus")),
                    p("See andmestik sisaldab linnahotelli ja kuurorthotelli broneeringu info nagu broneeringu tegemise aeg, viibimise kestus, täiskasvanute, laste ja/või imikute arv ning vabade parkimiskohtade arv ja mõned teised."),
                    p("Andmestik sisaldab broneeringuid, mis peavad saabuma ajavahemikul 1. juulist 2015 kuni 31. augustini 2017, sealhulgas broneeringuid, mis tegelikult saabusid, ja broneeringuid, mis oli tühistatud."),
                    br(),
                    p(strong("Andmestik: ")),
                    a("https://www.kaggle.com/datasets/jessemostipak/hotel-booking-demand", href="https://www.kaggle.com/datasets/jessemostipak/hotel-booking-demand"),
                    br(),
                    br(),
                    br(),
                    br(),
                    br(),
                    br(),
                    h5("Autor: Juri Garanin"),
                    
                      width = 3),
            mainPanel(
                tabsetPanel(
                  tabPanel("Sissejuhatus",
                           fluidRow(
                             column(8,
                          h3(strong("Hotelli broneeringute visualiseerimine")),
                           h4(strong("Projekti kirjeldus")),
                           br(),
                           h4("Projekti eesmärgid:"),
                           p("- Andmestiku ülevaade ja analüüs"),
                           p("- Visualiseerida hotelli broneeringute andmed riigi järgi"),
                          p("- Visualiseerida hotelli broneeringute statistika (kogu maailm)"),
                           p("- 'ITB8812 Andmete visualiseerimine' kursuses omandatud meetodite ja oskuste kasutamine"),
                           br(),
                           h4("Rakenduse struktuur:"),
                           h5(strong("Andmestik")),
                           p("Selles osas te võite leida andmestiku kirjeldus ja struktuur, ning korrelatsioonimatriks ja tunnuste diagrammid."),
                           h5(strong("Riigid")),
                           p("Selles osas on hotelli broneeringute graafikud riigi järgi:"),
                           p("- Hotellide broneerimise statistika"),
                           p("- Turustuskanalite võrdlus"),
                           p("- Keskmine päevahind hotelli järgi"),
                           
                           
                           h5(strong("Maailm")),
                           p("Selles osas on kogu maailma hotelli broneeringute statistika: "),
                           p("- Broneeringu staatus hotellitüübi järgi"),
                           p("- Broneeringu staatus kuu järgi"),
                           p("- Turustuskanalite võrdlus"),
                           p("- Broneeringute arv aasta järgi"),
                             )),
                  ),
                  tabPanel("Tunnused",
                           fluidRow(
                             column(8,
                           h4(strong("Tunnuste kirjeldus")),
                            )),
                           fluidRow(column(3,p(strong("hotel"))),column(8,p("Resort Hotel või City Hotel"))),
                           fluidRow(column(3,p(strong("is_canceled"))),column(8,p("Väärtus, mis näitab, kas broneering tühistati (1) või mitte (0)"))),
                           fluidRow(column(3,p(strong("lead_time"))),column(8,p("Päevade arv, mis jäi broneeringu sisestamise kuupäeva PMS-i ja saabumise kuupäeva vahele"))),
                           fluidRow(column(3,p(strong("arrival_date_year"))),column(8,p("Saabumiskuupäev (aasta)"))),
                           fluidRow(column(3,p(strong("arrival_date_month"))),column(8,p("Saabumiskuupäev (kuu)"))),
                           fluidRow(column(3,p(strong("arrival_date_week_number"))),column(8,p("Saabumiskuupäeva aasta nädala number"))),
                           fluidRow(column(3,p(strong("arrival_date_day_of_month"))),column(8,p("Saabumise päev"))),
                           fluidRow(column(3,p(strong("stays_in_weekend_nights"))),column(8,p("Nädalavahetuse ööde arv (laupäeval või pühapäeval), mil külaline viibis või broneeris hotellis ööbimise"))),
                           fluidRow(column(3,p(strong("stays_in_week_nights"))),column(8,p("Ööde arv nädalas (esmaspäevast reedeni), mil külaline hotellis peatus või broneeris"))),
                           fluidRow(column(3,p(strong("adults"))),column(8,p("Täiskasvanute arv"))),
                           fluidRow(column(3,p(strong("children"))),column(8,p("Laste arv"))),
                           fluidRow(column(3,p(strong("babies"))),column(8,p("Value indicating if the booking was canceled (1) or not (0)"))),
                           fluidRow(column(3,p(strong("meal"))),column(8,p("Broneeritud söögi tüüp"))),
                           fluidRow(column(3,p(strong("country"))),column(8,p("Päritoluriik. Kategooriad on esitatud ISO 3155–3:2013 formaadis"))),
                           fluidRow(column(3,p(strong("market_segment"))),column(8,p("Turusegmendi määramine. Kategooriates tähendab termin 'TA' reisibürood ja 'TO' tähendab reisikorraldajaid"))),
                           fluidRow(column(3,p(strong("distribution_channel"))),column(8,p("Broneerimise jaotuskanal"))),
                           fluidRow(column(3,p(strong("is_repeated_guest"))),column(8,p("Väärtus, mis näitab, kas broneeringu nimi pärineb korduvalt külaliselt (1) või mitte (0)"))),
                           fluidRow(column(3,p(strong("previous_cancellations"))),column(8,p("Varasemate broneeringute arv, mille klient tühistas enne praegust broneeringut"))),
                           fluidRow(column(3,p(strong("previous_bookings_not_canceled"))),column(8,p("Varasemate broneeringute arv, mida klient ei ole tühistanud enne praegust broneeringut"))),
                           fluidRow(column(3,p(strong("reserved_room_type"))),column(8,p("Broneeritud toatüübi kood. Anonüümsuse huvides esitatakse nimetuse asemel kood."))),
                           fluidRow(column(3,p(strong("assigned_room_type"))),column(8,p("Broneeringule määratud toatüübi kood."))),
                           fluidRow(column(3,p(strong("booking_changes"))),column(8,p("Broneeringus tehtud muudatuste/paranduste arv alates broneeringu PMS-i sisestamisest"))),
                           fluidRow(column(3,p(strong("deposit_type"))),column(8,p("Märge selle kohta, kas klient tegi broneeringu tagamiseks sissemakse."))),
                           fluidRow(column(3,p(strong("agent"))),column(8,p("Broneeringu teinud reisibüroo ID"))),
                           fluidRow(column(3,p(strong("company"))),column(8,p("Broneeringu teinud või broneeringu tasumise eest vastutava ettevõtte/üksuse ID."))),
                           fluidRow(column(3,p(strong("days_in_waiting_list"))),column(8,p("Päevade arv, mil broneering oli ootenimekirjas, enne kui see kliendile kinnitati"))),
                           fluidRow(column(3,p(strong("customer_type"))),column(8,p("Broneeringu tüüp, eeldades ühte neljast kategooriast: Leping – kui broneeringuga on seotud kvoot või muud tüüpi leping; Grupp – kui broneering on seotud grupiga; mööduv – kui broneering ei ole osa grupist või lepingust ega ole seotud muu mööduva broneeringuga; Transient-party – kui broneering on mööduv, kuid on seotud vähemalt muu mööduva broneeringuga"))),
                           fluidRow(column(3,p(strong("adr"))),column(8,p("Keskmine päevahind, mis on määratud kõigi majutustehingute summa jagamisel ööbimiste koguarvuga"))),
                           fluidRow(column(3,p(strong("required_car_parking_spaces"))),column(8,p("Kliendi poolt vajalike autode parkimiskohtade arv"))),
                           fluidRow(column(3,p(strong("total_of_special_requests"))),column(8,p("Kliendi erisoovide arv (nt kaheinimesevoodi või kõrge korrus)"))),
                           fluidRow(column(3,p(strong("reservation_status"))),column(8,p("Broneeringu viimane olek, eeldades ühte kolmest kategooriast: Canceled – klient tühistas broneeringu; Check-Out – klient on sisse registreerinud, kuid juba lahkunud; No-Show – klient ei registreerunud ja teatas hotellile põhjusest, miks calendar_today"))),
                           fluidRow(column(3,p(strong("reservation_status_date"))),column(8,p("Kuupäev, mil viimane olek määrati."))),
                  ),
                  tabPanel("Andmed",
                    h4(strong("Tabel")),
                    dataTableOutput("tabel")
                  ),
                  tabPanel("Struktuur",
                           h4(strong("Andmestiku kirjeldus")),
                           fluidRow(
                             column(12,
                                    p("Andmestikus on ", strong("119390")," objektid, mida kirjeldavad", strong("32"), "tunnused. "),
                                    verbatimTextOutput("str"),
                                    br()
                             )),
                           fluidRow(
                             column(12,
                                    
                               verbatimTextOutput("skim"),
                               p("Puuduvad värtused:", strong("children: 4")),
                             ))
                  ),
                  tabPanel("Korelatsioon",
                           h4(strong("Korrelatsioonimaatriksi visualiseerimine: ")),
                           fluidRow(
                             column(8,
                                    radioButtons("rb", "Graafiku tüüp:",
                                                 choiceNames = list(
                                                   "Number",
                                                   "Color",
                                                   "Circle"
                                                 ),
                                                 choiceValues = list(
                                                   "number", "color", "circle"
                                                 ), inline=T)
                             )
                           ),
                           fluidRow(
                             column(8,
                                    plotOutput("corr_plot",width = "800", height = "800px")
                             )
                           )
                           ),
                  tabPanel("Tunnuste diagrammid",
                           h4(strong("Tunnused: ")),
                           selectInput(
                             "objects_list",
                             "",
                             int_objects_list,
                             selected = "lead_time",
                             multiple = FALSE,
                             selectize = TRUE,
                             width = NULL,
                             size = NULL
                           ),
                           h4("Sageduse diagramm: "),
                           fluidRow(
                             column(8,
                                    plotOutput("histPlot")   
                             )
                           ),
                           fluidRow(
                             column(8,
                                h4("Karp-vurruv diagramm: "),
                                plotOutput("boxPlot")   
                             )
                           ),
                  )
                )
  ))),
  tabPanel("Riigid",
           sidebarLayout(
             sidebarPanel(
               img(src = "booking.png", height = "100%", width = "100%"),
               h4(strong("Graafikud (riigi järgi)")),
               selectInput(
                 "selected_country",
                 "Valige riik: ",
                 countres_list,
                 selected = NULL,
                 multiple = FALSE,
                 selectize = TRUE,
                 width = NULL,
                 size = NULL
               ),
               
               width = 3),
             mainPanel(
               tabsetPanel(
                 tabPanel("Broneerimine hotellitüübi järgi",
                          fluidRow(
                            column(8,
                          h4(strong("Hotellide broneerimise statistika")),
                          plotOutput("reservation_status_plot"),
                          htmlOutput("hotels_count")
                          )),
                 ),
                 tabPanel("Turustuskanal",
                          fluidRow(
                            column(8,
                                   h4(strong("Turustuskanalite võrdlus")),
                          plotOutput("channel_plot_country")),
                          )),
                 tabPanel("Hinnad",
                          fluidRow(
                            column(8,
                                   h4(strong("Keskmine päevahind hotelli järgi")),
                          plotOutput("hotel_price_by_country")
                            ))
           ))))),
  tabPanel("Maailm",
  sidebarLayout(
    sidebarPanel(
      h4(strong("Maailma statistika")),
      h5("Broneeringu staatus hotellitüübi järgi"),
      p("Kõigist tehtud broneeringutest enam kui 66% moodustasid City hotellide broneeringupäringud ja umbes 34% kuurorthotellide broneeringud."),
      br(),
      h5("Broneeringu staatus kuu järgi"),
      p("Enamik hotellibroneeringute taotlusi saabus juulis ja augustis, millele järgnesid mais ja oktoober. Selle üheks põhjuseks võib olla ilmastikumõju."),
      br(),
      h5("Turustuskanalite võrdlus"),
      p(strong("TA/TO"), "on enim broneeringute arv.", strong("TA "), "tähendab reisibüroo, ", strong("TO "), "tähendab reisikorraldajaid.
        Online TA sai teistest segmentidest kõrgeimaks turusegmendiks."),
      br(),
      h5("Broneeringute arv aasta järgi"),
      p("Graafikult on näha, et selles andmestikus oli tehtud kõige rohkem broneeringuid 2016. aastal"),
      width = 3),
      
    mainPanel(
      fluidRow(
        column(6,
               h4(strong("Broneeringu staatus hotellitüübi järgi", style = "margin-left: 5%")),
               br(),
               plotOutput("hotels_pie")
               ),
        column(6,
               h4(strong("Broneeringu staatus kuu järgi"), style = "margin-left: 5%;"),
               br(),
               plotOutput("reservation_by_month_plot")
               )
      ),
      br(),
      fluidRow(
        column(6,
               h4(strong("Turustuskanalite võrdlus"), style = "margin-left: 5%;"),
               br(),
               plotOutput("channel_plot_world")
        ),
        column(6,
               h4(strong("Broneeringute arv aasta järgi"), style = "margin-left: 5%;"),
               br(),
               plotOutput("bookings_by_year_world")
        )
      )
      )))
)

# Server logic
server <- function(input, output) {
  
  country_input <- reactive({
    country_code <- countrycode(input$selected_country,"country.name", "iso3c")
    x <- bookings[bookings$country == country_code,]
  })
  
  info <- reactiveValues()
  observe({
    info$country_name <- input$selected_country
    info$city_count <- count(bookings[bookings$country == countrycode(input$selected_country,"country.name", "iso3c") & bookings$hotel == "City Hotel",])
    info$resort_count <- count(bookings[bookings$country == countrycode(input$selected_country,"country.name", "iso3c") & bookings$hotel == "Resort Hotel",])
  })  
  
  output$tabel <- renderDataTable( bookings, options = list(searching = FALSE,ordering=F, lengthMenu = c(5, 10, 20), pageLength = 10,scrollX = TRUE))
  output$skim <- renderPrint({skim(bookings)})
  output$str <- renderPrint({str(bookings)})
  
  output$distPlot <- renderPlot({
    
    hist(bookings$arrival_date_week_number)
  })
  
  output$corr_plot <- renderPlot({
    data_set <- na.omit(bookings)
    corrplot(cor(Filter(is.numeric, data_set)),method=input$rb)
  })
  
  output$histPlot <- renderPlot({
    hist(unlist(bookings[input$objects_list]), freq=TRUE, xlab=input$objects_list, main=sprintf("Tunnuse %s \n sageduse diagramm", input$objects_list), cex.main=1,cex.axis=1,cex.lab=1)
    })
  
  output$boxPlot <- renderPlot({
    boxplot(bookings[input$objects_list], col="skyblue", horizontal=1, main=sprintf("Tunnuse %s \n karp-vurrud diagramm", input$objects_list),cex.main=1, cex.axis=1,cex.lab=1)
  })
  
  
  # Reservation status by hotel type by country
  output$reservation_status_plot <- renderPlot({
    
    ggplot(data = country_input(),
           aes(
             x = hotel,
             y = prop.table(stat(count)),
             fill = factor(is_canceled),
             label = scales::percent(prop.table(stat(count)))
           )) +
      geom_bar(position = position_dodge()) +
      geom_text(
        stat = "count",
        position = position_dodge(.9),
        vjust = -0.2,
        size = 5
      ) +
      scale_y_continuous(labels = scales::percent) +
      labs(title = sprintf("Broneeringu staatus hotellitüübi järgi (%s) ", info$country_name), x = "Hotelli tüüp", y = "Count") +
      theme_classic(base_size = 14) +
      scale_fill_discrete(
        name = "Broneeringu staatus",
        breaks = c("0", "1"),
        labels = c("Pole tühistatud", "Tühistatud")
      )
    
    })
  
  
  # World Tab -> reservations by month
  output$reservation_by_month_plot <- renderPlot({
    
    datatest1 <-
      bookings %>% 
      mutate(arrival_date_month = factor(
        arrival_date_month,
        levels = c(
          "January",
          "February",
          "March",
          "April",
          "May",
          "June",
          "July",
          "August",
          "September",
          "October",
          "November",
          "December"
        ),
        ordered = TRUE
      ))
    
    ggplot(datatest1, aes(arrival_date_month, fill = factor(is_canceled))) +
      geom_bar() +
      geom_text(stat = "count",
                aes(label = ..count..),
                position = position_stack(vjust = 0.5)) +
      scale_fill_discrete(
        name = "Broneeringu staatus",
        breaks = c("0", "1"),
        label = c("Pole tühistatud", "Tühistatud")
      ) +
      labs(title = "", x = "Kuu", y = "Arv") +
      scale_x_discrete(labels = month.abb) +
      theme_classic(base_size = 15)
    
  })
  
  # Reservation status by hotel type
  output$hotels_pie <- renderPlot({
    
    ggplot(data = bookings,aes(hotel,fill=hotel,label = scales::percent(prop.table(stat(count)))))+
      geom_bar()+
      geom_text(
        stat = "count",
        position = position_dodge(.9),
        vjust = -0.2,
        size = 4
      ) +
      scale_y_continuous(labels = scales::percent)+
      theme_classic(base_size = 15)
    
  })
  
  # distribution_channel by country
  output$channel_plot_country <- renderPlot({
    
    ggplot(data = country_input()) +
      geom_bar(mapping = aes(x = distribution_channel, fill = market_segment)) +
      theme_classic(base_size = 16)

  })
  
  # distribution_channel world
  output$channel_plot_world <- renderPlot({
    ggplot(data = bookings) +
      geom_bar(mapping = aes(x = distribution_channel, fill = market_segment)) +
      labs(title = "Grupeerimine turusegmendi järgi(market_segment)", subtitle = "") +
      theme_classic(base_size = 15)
    
  })
  
  # Average daily rate by Hotel Type (World)
  output$hotel_price_world <- renderPlot({
    
    ggplot(bookings, aes(x = adr, fill = hotel, color = hotel)) + 
      geom_histogram(aes(y = ..density..), position = position_dodge(), binwidth = 20 ) +
      geom_density(alpha = 0.2) + 
      labs(title = "",
           x = "Hotelli hind (eurodes)",
           y = "Arv") + scale_color_brewer(palette = "Paired") + 
      theme_classic(base_size = 14) + theme(legend.position = "top")
    
  })
  
  # Average daily rate by Hotel Type by Country
  output$hotel_price_by_country <- renderPlot({
    
    ggplot(country_input(), aes(x = adr, fill = hotel, color = hotel)) + 
      geom_histogram(aes(y = ..density..), position = position_dodge(), binwidth = 20 ) +
      geom_density(alpha = 0.5) + 
      labs(title = "",
           x = "Hotelli hind (eurodes)",
           y = "") + scale_color_brewer(palette = "Paired") + 
      theme_classic(base_size = 14) + theme(legend.position = "top")
    
  })
  
  # bookings by year world
  output$bookings_by_year_world <- renderPlot({

    ggplot(data = bookings,aes(arrival_date_year,label = scales::percent(prop.table(stat(count)))))+
      geom_bar()+
      geom_text(
        stat = "count",
        position = position_dodge(.9),
        vjust = -0.2,
        size = 6
      ) +
      theme_classic(base_size = 15)
    
  })
  
  
  output$hotels_count <- renderText({
    paste("<br><br><b>Broneeringute arv</b><br><br>", "Kuurorthotell: ", info$resort_count, "<br>Linna hotell: ", info$city_count,
          "<br><br><b>Kokku: ", info$resort_count+info$city_count, "</b>" )
  })
}


  
  
# Run the application 
shinyApp(ui = ui, server = server)

