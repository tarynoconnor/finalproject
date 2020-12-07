#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(rstanarm)
library(shinyWidgets)
library(tidymodels)
library(shinythemes)


# First, I read all the .RDS files that I saved from my markdown file

plot_1 = readRDS("plot_1.RDS")
model_2 = readRDS("model_2.RDS")
contribution_votes = readRDS("contribution_votes.RDS")
votes = readRDS("votes.RDS")
votes_test = readRDS("votes_test.RDS")
votes_train = readRDS("votes_train.RDS")

ui <- navbarPage(
    "Analysis of US Senate Elections, 1980 - 2018",
    theme = shinytheme("flatly"),
    
# First, I included my trend graphs, such as the national change, and how much
# winners outspent their opponents 
    
    tabPanel("Trends",
             fluidPage(
                 titlePanel("Trends in Spendings on US Senate Campaigns"),
                 sidebarLayout(
                     sidebarPanel(
                         helpText("Select a state to see how much winning Senate 
                                  campaigns outspent their opponents."),
                         
# For the outspending graph, I added an input by which people can select a state
# to see the data only for that state.
                         
                         selectInput("state",
                                     label = "State:",
                                     choices = state.name,
                                     selected = "Alabama"
                                     )
                         ),

# I then inserted the graph with statewide spending margins and national spending

                 mainPanel(column(12, fluidRow(plotOutput("disbursement_win")
                                               )
                                  ),
                           column(12, fluidRow(plotOutput("national_trend")
                                               )
                                  ),
                           p("The graph above shows how while Democratic and
                             Republican candidates were spending rounghly the
                             same amount of money on their campaigns, a gap can
                             now be seen beginning to form between candidates 
                             from the two parties. Both parties have seen an 
                             increase in campaign funding, but Democratic campaigns
                             are beginning to spend on average more on their
                             campaigns. But does this result in more victories?"),
                           )
                 )
                 )
             ),

# I then added a tab with my distribution of vote percentages for incumbents,
# challengers, and candidates filling open seats

    tabPanel("Candidate Types",
             fluidPage(
                     titlePanel("Distribution of Votes for Different Types of Candidates"),
                     sidebarLayout(
                         sidebarPanel(
                             helpText(""),
                             
# For graphs with a year input, I chose to have a slider instead of a drop-down
                             
                             sliderTextInput("year",
                                         label = "Choose a year:",
                                         choices = c("1980", "1982", "1984", 
                                                     "1986", "1988", "1990", 
                                                     "1992", "1994", "1996", 
                                                     "1998", "2000", "2002", 
                                                     "2004", "2006", "2008", 
                                                     "2010", "2012", "2014",
                                                     "2016", "2018")
                                         )
                             ),
                         mainPanel(fluidRow(plotOutput("candidate_status")
                                            )
                                   )
                         )
                     )
             ),

# I then included my model of the relationship between campaign expenditures and
# vote percentages, and my analysis of said model.

    tabPanel("Model",
             fluidPage(
                 titlePanel("Linear Relationship between Campaign Expenditures and Percent of Votes"),
                          sidebarLayout(
                              sidebarPanel(
                                  helpText(""),
                                  sliderTextInput("year_2",
                                                  label = "Choose a year:",
                                                  choices = c("1980", "1982", "1984", 
                                                              "1986", "1988", "1990", 
                                                              "1992", "1994", "1996", 
                                                              "1998", "2000", "2002", 
                                                              "2004", "2006", "2008", 
                                                              "2010", "2012", "2014",
                                                              "2016", "2018") ,
                                                  selected = 1980)
                                  ),
                              
                          mainPanel(
                              fluidRow(column(12, plotOutput("linear")
                                              )
                                       ),
                              fluidRow(column(12, align = "center",plotOutput("lin_reg")
                                              )
                                       ),
                              p("While the linear model consistently shows a positive
                                relationship between expenditures and the percent of 
                                the vote earned by a Seante campaign, the metrics 
                                from this model suggest that this is a weak relationship,
                                with R squared being only .3. The analysis model above,
                                which compares the predictions of the linear model 
                                of the data to the actual outcomes of the elections,
                                it can clearly be seen that there is a lot more 
                                variance in the actual outcomes than the predicted 
                                outcomes. It is likely that a non-linear model will
                                better represent the relationship between disbursements 
                                and campaign performance, if there is a relationship.")
                              )),
                 )
             ),

# I then added my about tab

    tabPanel("About",
             titlePanel("About"),
             h3("Background"),
             p("This project aims to analyze the factors that play a role in the success of American
               Senate campaigns dating back to 1980, including reported campaign
               expenditures and whether the Senate seat being contested is open or not. 
               It was largely motivated by the recent increase in campaign 
               contributions and expenditures in highly contested Senate races, such as
               the 2020 South Carolina race between Jaime Harrison and Lindsey Graham,
               in which Harrison raised $40 million dollars more than Graham, but lost 
               by a margin of 10 percent."),
             h3("Data"),
             p("The data used for this project was collected from the ", 
               a(href = "https://www.fec.gov/data/candidates/senate/?election_year=2018&cycle=2018&election_full=true&is_active_candidate=true",
               "Federal Election Commission"), "and the ", a(href = "https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/PEJ5QU",
                                                            "MIT Election Lab.")),
             h3("About Me"),
             p("My name is Taryn O'Connor, and I am a first-year at Harvard College
               planning to study Applied Mathematics. You can reach me at 
               tarynoconnor@college.harvard.edu.
               
               Feel free to check out the GitHub repository for this project ", 
               a(href = "https://github.com/tarynoconnor/finalproject", "here."))))
    
    



# Graph codes

server <- function(input, output) {
    
# I took the code for the outspending graph and inserted it here
    
    output$disbursement_win <- renderPlot({
        plot_1 %>%  
            
# The data was filtered to whatever state was selected in the drop-down menu
            
            filter(state.x == input$state) %>%
            ggplot(aes(x = year, y = diff/1000000, color = party)) +
            geom_point(size = 5) +
            
# I changed the color scheme of the graph to have the points also reflect the
# winning party
            
            scale_color_manual(breaks = c("DEM", "REP", "IND"),
                values = c("DEM" = "blue", "REP" = "red", "IND" = "green"), 
                               labels = c("Democrat", "Republican", "Independent")) +
            labs(title = "How Much Winning Campaigns Outspent Opponents",
                x = "Election Year",
                y = "Spending Margin (in Millions of Dollars)",
                color = "Party") +
            scale_x_discrete(breaks = c(1980, 1982, 1984, 1986, 1988, 1990, 
                                          1992, 1994, 1996, 1998, 2000, 2002, 
                                          2004, 2006, 2008, 2010, 2012, 2014, 
                                          2016, 2018)) +
            theme_bw() +
            
# I also changed the y-axis to have percentage values and the size of the text 
# to make the graph readable.
            
            scale_y_continuous(labels = scales::comma) +
            theme(text = element_text(size = 15), 
                  axis.text = element_text(size = 15), 
                  axis.title = element_text(size = 15),
                  plot.title = element_text(hjust = .5))
    })
    
# This is the posterior distribution of vote percentages for each type of 
# candidate
    
    output$candidate_status <- renderPlot({
        input_plot <- as.character(input$year)
        model_2 %>% 
            rename(year = as.character(input_plot)) %>% 
            ggplot(aes(x = year, y = after_stat(count/sum(count)), 
                       fill = candidate_status)) +
            geom_histogram(bins = 100,
                           alpha = .5,
                           position = "identity") +
            theme_classic() +
            
# I took the code from the .rmd and added appropriate labels and aesthetics, 
# such as font size 
            
            labs(title = "Posterior Distribution of Vote Percentages for Candidates",
                 subtitles = "Incumbents are consistently likely to win elections",
                 x = "Percent of Votes",
                 y = "Probability",
                 fill = "Type of Candidate") +
            scale_x_continuous(labels = scales::percent_format(scale = 1)) +
            scale_y_continuous(labels = scales::percent_format()) +
            scale_fill_manual(breaks = c("challenger", "incumbent", "open_seat"),
                                values = c("challenger" = "red", "incumbent" = 
                                               "blue", "open_seat" = "#FFCC33"), 
                                labels = c("Challenger", "Incumbent", "Open Seat")) +
            theme(text = element_text(size = 15), 
                  axis.text = element_text(size = 15), 
                  axis.title = element_text(size = 15),
                  plot.title = element_text(hjust = .5))
        
    })
    
# This is the national trend in expenditures, using the exact code from the .rmd
    
    output$national_trend <- renderPlot({
        contribution_votes %>% 
            mutate(year = as.numeric(year)) %>% 
            filter(party == "DEM" | party == "REP") %>% 
            ggplot(aes(x = year, y = disbursements/1000, color = party)) + 
            geom_point(alpha = .3) +
            geom_smooth() +
            
# Again, appropriate aesthetics were added. 
            
            scale_color_manual(values = c("DEM" = "blue", "REP" = "red"), 
                               labels = c("Democrat", "Republican")) +
            labs(title = "Nationwide Expenditures on Democratic and Republican Campaigns",
                 subtitle = "Average spendings have been increasing",
                 x = "Year",
                 y = "Spendings (in Millions of Dollars)",
                 color = "Party") +
            theme_classic() +
            theme(text = element_text(size = 15), 
                  axis.text = element_text(size = 15), 
                  axis.title = element_text(size = 15),
                  plot.title = element_text(hjust = .5)) +
            scale_x_continuous(breaks = c(1980, 1990, 2000, 2010, 2018))
    })
    
# This is the linear model analysis code 
    
    output$lin_reg <- renderPlot({
        
# The input from the slider was saved into an object
        
        input_plot <- input$year_2
        
        workflow() %>% 
            add_model(linear_reg() %>%
                          set_engine("stan") %>%
                          set_mode("regression")) %>% 
            add_recipe(recipe(vote_percentage ~  disbursements,
                              
# The data was filtered based on the input
                              
                              data = votes_train %>% filter(year == input_plot)) %>% 
                           step_dummy(all_nominal())) %>% 
            fit(data = votes_train %>% filter(year == input_plot)) %>% 
            predict(new_data = votes_train %>% filter(year == input_plot)) %>% 
            bind_cols(votes_train %>% filter(year == input_plot) %>% select(vote_percentage)) %>% 
            ggplot(aes(x = vote_percentage, y = .pred)) +
            geom_point(size = 3) +
            theme_bw() +
            
# Again, the code from the .rmd was used and appropriate aesthetics were added
            
            labs(title = "Analysis of Linear Relationship",
                 x = "Actual Percent of Vote Received",
                 y = "Predicted Percent of Vote Received") +
            theme(text = element_text(size = 15), 
                  axis.text = element_text(size = 15), 
                  axis.title = element_text(size = 15),
                  plot.title = element_text(hjust = .5, face = "bold")) +
            scale_x_continuous(labels = scales::percent_format(scale = 1)) +
            scale_y_continuous(labels = scales::percent_format(scale = 1))
    })
    
# This is the linear model graph, again using the input from the slider to filter
# the code to make a model for a given year.
    
    output$linear <- renderPlot({
        data <- votes %>% 
            filter(year == input$year_2)
        model_votes <- stan_glm(data = data,
                                vote_percentage ~ disbursements,
                                family = gaussian(),
                                refresh = 0)
        
 
# The code from the .rmd was used and again, titles and other aesthetics were 
# added.
               
        data %>% 
            ggplot(aes(x = disbursements, y = vote_percentage)) +
            geom_point() +
            geom_line(aes(y = fitted(model_votes)), color = "red") +
            theme_classic() +
            labs(x = "Expenditures (in Thousands of Dollars)",
                 y = "Percent of Vote Received") +
            theme(text = element_text(size = 15), 
                  axis.text = element_text(size = 15), 
                  axis.title = element_text(size = 15),
                  plot.title = element_text(hjust = .5)) +
            scale_y_continuous(labels = scales::percent_format(scale = 1))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
