

# Hämtar data och utför databearbetning


library(dplyr)
library(olsrr)
library(car)
library(kableExtra)

# Laddar ner data

library(readr)
fotbollSpelare <- read_delim("~/Downloads/fotbollSpelare.csv", 
                             delim = ";", locale = locale(decimal_mark = ",") )

fs <- fotbollSpelare


# Skapar index för att plocka ut de observationer med positionen anfallare

index <- grep("ST \\(C\\)", fs$Position) 

ST <- fs[index,]




# Plockar ut responsvariaben eftersom den är med kolon i ST 

respons <-  as.numeric(gsub( ",", ".", ST$xG))


# Skapar index för hur många straffar de tagit

straff1 <- (ST$`Penalties Taken` == 1)

straff2 <- (ST$`Penalties Taken` == 2)

straff3 <- (ST$`Penalties Taken` == 3)

straff4 <- (ST$`Penalties Taken` == 4)

straff5 <- (ST$`Penalties Taken` == 5)

straff6 <- (ST$`Penalties Taken` == 6)


# Tar bort rätt antal XG värde

respons[straff1] <- respons[straff1] - 0.83

respons[straff2] <- respons[straff2] - (0.83 * 2)

respons[straff3] <- respons[straff3] - (0.83 * 3)

respons[straff4] <- respons[straff4] - (0.83 * 4)

respons[straff5] <- respons[straff5] - (0.83 * 5)

respons[straff6] <- respons[straff6] - (0.83 * 6)



# Skapar XG per 90 min 

# Plockar ut spelade minuter och dividerar non penalty xg med det 

np_xg_per90 <- ( respons / ST$`Minutes Played` ) * 90


# Lägger in den nya responsvariabeln i ST


ST$respons <- np_xg_per90

#-------------------------------#



# Väljer ut variabler av intresse

fotboll <- ST %>% select( 
  
  Acc, Agg,Agi,Ant, 
  
  Bal,Bra,Cmp,Cnt, 
  
  Dec,Det,Dri,Fin, 
  
  Fir,Fla,Hea,Jum, 
  
  Ldr,Lon,OtB,Pac,Pas,Pos, 
  
  Str,Tck,Tea,Tec,Vis, 
  
  Wor, respons, Name, `Minutes Played`)  



# Summerar per namn


avg_data <- fotboll %>% 
  
  group_by(Name) %>% 
  
  summarise_all(mean, na.rm = TRUE) 



# Plocka bort de observationer med för få spelade minuter så borde det blir ännu
# mer normalfördelat. 


min_played_index <- (avg_data$`Minutes Played` >450 )


# Indexerar bort de med för lite spelade minuter 

avg_data<- avg_data[min_played_index,]

# Plockar bort spelade minuter

avg_data <- avg_data[,-31]

avg_data <- avg_data[,-1]

# Skapa modeller
modell1 <- lm(respons ~ Fin + Acc + Str + Pas + Jum + Wor + Det + Cmp + OtB, data = avg_data)
modell2 <- lm(respons ~ Fin + Acc + Str + Pas, data = avg_data)


#------------------------------------------------------------------------#

              # Gör applikationen

library(shiny)




# Lista av variabler att välja mellan
variable_choices <- colnames(avg_data)[-ncol(avg_data)]  # Tar bort xG 

ui <- fluidPage(
  titlePanel("Projektarbete"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("xvar", "Välj en variabel för x-axeln:", choices = variable_choices),
      radioButtons("model", "Välj en modell:", choices = c("Modell 1", "Modell 2"), selected = 'Modell 1'),
      radioButtons("outputType", "Välj utdata:", choices = c("Parametrar", "Förklaringsmått"), selected = "Parametrar")
    ),
    
    mainPanel(
      h3('Spridningsdiagram'),
      plotOutput("scatterPlot"), 
      textOutput("correlationText"),
      conditionalPanel(
        condition = "input.model == 'Modell 1'",
        h3("Modell 1")
      ),
      conditionalPanel(
        condition = "input.model == 'Modell 2'",
        h3("Modell 2")
      ),
      tableOutput("outputTable")
    )
  )
)

server <- function(input, output) {
  
  output$scatterPlot <- renderPlot({
    ggplot(data = avg_data, aes_string(x = input$xvar, y = "respons")) +
      geom_point() +
      theme_bw() +
      geom_smooth(method = 'lm', se = FALSE) +
      ylab("xG")
  })
  
  output$correlationText <- renderText({
    selected_var <- input$xvar
    correlation <- cor(avg_data[[selected_var]], avg_data$respons)
    paste("Korrelationen mellan", selected_var, "och xG är:", round(correlation, 2))
  })
  
  output$outputTable <- renderTable({
    model <- if (input$model == "Modell 1") modell1 else modell2
    if (input$outputType == "Parametrar") {
      summod <- summary(model)
      summod_c <- summod$coefficients
      summod_c <- data.frame(summod_c)
      summod_c <- round(summod_c, 4)
      summod_c
    } else {
      summod <- summary(model)
      r_mod <- data.frame(summod$r.squared, summod$adj.r.squared, AIC(model))
      colnames(r_mod) <- c('R^2', 'Adj R^2', 'AIC')
      round(r_mod, 3)
    }
  }, rownames = TRUE)
  
}

# Kör Shiny-applikationen
shinyApp(ui = ui, server = server)



