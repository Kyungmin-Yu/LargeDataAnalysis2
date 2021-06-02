library(data.table)
library(shiny)
library(tidyverse)
library(lubridate)
# setwd("C:/Users/ykm25/Desktop/대용량자료관리및시각화/project/shiny/gu")
traffic = fread('traffic.csv') 
rain = fread('rain.csv') %>% mutate(date = ymd(date))
accident = fread('accident.csv')

ui = fluidPage(
  titlePanel("자치구별 사고일의 변수 시각화하기"),

  fluidPage(
    helpText("서울시 자치구별 교통량, 강수량, 사고의 정보를 시각화"),

    selectInput("gu",
                label = "자치구를 선택하세요",
                choices = traffic$시군구명 %>% unique(),
                selected = "종로구"),

    selectInput("Date",
                label = "날짜를 선택하세요",
                choices = traffic$일자 %>% unique(),
                selected = "2017-01-01")),
  mainPanel(
    fluidRow(
      splitLayout(style = "border: 1px solid silver:", cellWidths = c(600,600,600),
                  plotOutput("plotgraph1"),
                  plotOutput("plotgraph2"),
                  plotOutput("plotgraph3")
      )
  )
))


server = function(input, output){
  
  
  pt1 =reactive({
    return( traffic %>% 
    mutate(일자 = ymd(일자)) %>% 
    left_join(rain, by=c('일자'='date', 'hour'='hour')) %>%
    filter(일자 == input$Date & 시군구명 == input$gu) %>% 
    ggplot()+
    geom_line(aes(x=hour, y=교통량), color="red")+
    theme(panel.background = element_rect(fill='white', color='black', linetype='solid'),
          strip.background = element_rect(fill="white", color="black"),
          plot.title = element_text(size=15))+
    labs(x="시간", y="교통량", title = paste0(input$gu, "의 교통량")))})
  
  pt2 = reactive({ return( traffic %>% 
    mutate(일자 = ymd(일자)) %>% 
    left_join(rain, by=c('일자'='date', 'hour'='hour')) %>%
    filter(일자 ==  input$Date) %>% 
    ggplot()+
    geom_line(aes(x=hour, y=강수량), color="blue")+
    theme(panel.background = element_rect(fill='white', color='black', linetype='solid'),
          strip.background = element_rect(fill="white", color="black"),
          plot.title = element_text(size=15))+
    labs(x="시간", y="강수량", title = paste0(input$gu, "의 강수량")))})
  
  pt3 =reactive({return( accident %>%
    filter(date == input$Date & 시군구명 == input$gu) %>% 
    group_by(시군구, 법정동) %>% 
    summarise(사고수 = n()) %>% 
    ggplot()+
    geom_bar(aes(x=법정동, y= 사고수, fill=시군구),stat = "identity")+
    theme(panel.background = element_rect(fill='white', color='black', linetype='solid'),
          axis.title.y=element_blank(),
          strip.background = element_rect(fill="white", color="black"),
          legend.position = "none",
          axis.text.x = element_text(size=10, colour = "black"))+
        labs(title = paste0(input$gu, "의 법정동별 사고건수")))})
  
  output$plotgraph1 = renderPlot({pt1()})
  output$plotgraph2 = renderPlot({pt2()})
  output$plotgraph3 = renderPlot({pt3()})
}

shinyApp(ui, server)
