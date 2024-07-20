library(shiny)
library(dplyr)
library(plotly)

grd <- read.csv("tamu_grade_reports.csv")

#Settings for Plot (Need to order semesters since they are categorical)
xaxis_order <- list(categoryorder = "array",
                    categoryarray = c("2017 Fall", 
                                      "2018 Spring", "2018 Summer", "2018 Fall",
                                      "2019 Spring", "2019 Summer", "2019 Fall",
                                      "2020 Spring", "2020 Summer", "2020 Fall",
                                      "2021 Spring", "2021 Summer", "2021 Fall",
                                      "2022 Spring", "2022 Summer", "2022 Fall",
                                      "2023 Spring", "2023 Summer", "2023 Fall",
                                      "2024 Spring"),
                    tickangle = -45)

ui <- fluidPage(
  titlePanel("TAMU Grade Distributions 2017-2024"),
      tags$a(href="https://github.com/ross-wgh/tamu-grades", "View this project on GitHub"),
      textInput("department","Select Department (Eg: ENGR, STAT, CSCE)"),
      textInput("course","Select Course Number (Eg: 101, 211, 640)"),
      submitButton("Submit"),
      plotly::plotlyOutput('course_plot'),
      tableOutput('course_table')
    )

server <- function(input, output, session) {
  data <- reactive({
    course_name <- paste(toupper(input$department), sep='-', input$course)
    course_data <- grd %>% 
      filter(Course == course_name) %>% 
      select('Course', 'Semester', 'Section', 'Instructor', 'GPA',
             'A','B','C','D','F', 'Total_Completed', 
             'I','S', 'U', 'Q','X', 'Total_Registered', 'Term.Year') %>%
      arrange(Term.Year)
    return(course_data)
  })
  
  plot_data <- function(df){
    course_plot <- df %>% group_by(Instructor, Semester, Term.Year, Course) %>% 
      summarize(total_students = sum(Total_Completed), term_gpa = sum(GPA * Total_Completed)/total_students) %>%
      arrange(desc(Term.Year))
    
    course_plot %>% 
      ungroup() %>%
      plot_ly(x = ~Semester, y = ~term_gpa,  type = 'scatter', color = ~Instructor, mode = 'lines+markers', width = 1200) %>%
      layout(xaxis = xaxis_order, yaxis = list(title = 'GPA'))
  }
  
  output$course_plot <- plotly::renderPlotly({
    plot_data(data())
  })
  
  output$course_table <- renderTable({
    data() %>% arrange(desc(Term.Year), Instructor, Section) %>% select(-Term.Year)
  })
}

shinyApp(ui, server)
