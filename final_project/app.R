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
library(gt)
library(gtsummary)


# First, I read all the .RDS files that I saved from my markdown file

plot_1 = readRDS("plot_1.RDS")
model_2 = readRDS("model_2.RDS")
contribution_votes = readRDS("contribution_votes.RDS")
votes = readRDS("votes.RDS")
votes_test = readRDS("votes_test.RDS")
votes_train = readRDS("votes_train.RDS")

ui <- navbarPage(
    "The Role of Campaign Finance in US Senate Elections, 1980 - 2018",
    theme = shinytheme("flatly"),
    
# First, I included my trend graphs, such as the national change, and how much
# winners outspent their opponents 
    
    tabPanel("Trends by Party",
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
                           p("As seen in the graph above, successful campaigns 
                             from 1980 - 2018 have won with different spending margins,
                             with some barely outspending their opponents, with others
                             being outspent by millions of dollars. However, while
                             there appears to be no particular trend in which the campaign
                             that spends the most wins, this project aims to look at 
                             how campaign expenditures are related to the amount 
                             of support different candidates receive and how
                             divided voters are at the polls."),
                           column(12, fluidRow(plotOutput("national_trend")
                                               )
                                  ),
                           p("The graph above shows how while Democratic and
                             Republican candidates were spending roughly the
                             same amount of money on their campaigns, a gap can
                             now be seen beginning to form between candidates 
                             from the two parties. Both parties have seen an 
                             increase in campaign funding, but Democratic campaigns
                             are beginning to spend more on average than Republican
                             campaigns. But does this result in more better performance
                             at the polls?"),
                           )
                 )
                 )
             ),

# I then included my model of the relationship between campaign expenditures and
# vote percentages, and my analysis of said model.

    tabPanel("Model",
             fluidPage(
                 titlePanel("Relationship between Campaign Expenditures and Percent of Votes"),
                          sidebarLayout(
                              sidebarPanel(
                                  width = 3,
                                  helpText("Choose a year to see the linear and 
                                           local regression models of the relationship 
                                           between campaign expenses and the 
                                           percentage of votes received."),
                                  sliderTextInput("year_2",
                                                  
# I used a slider input for this model
                                                  
                                                  label = "Year:",
                                                  choices = c("1980", "1982", "1984", 
                                                              "1986", "1988", "1990", 
                                                              "1992", "1994", "1996", 
                                                              "1998", "2000", "2002", 
                                                              "2004", "2006", "2008", 
                                                              "2010", "2012", "2014",
                                                              "2016", "2018") ,
                                                  selected = 1980)
                                  ),

# I also organized this page so that the models would be first and next to each
# other, so they could be compared. I then put my analysis of the linear model
# underneath, paired with a metrics table.

                          mainPanel(
                              fluidRow(splitLayout(cellWidths = c("50%", "50%"), 
                                                   plotOutput("linear"),
                                                   plotOutput("loess")
                                                   )
                                       ),
                              h2("Analysis of Linear Model"),
                              fluidRow(splitLayout(cellWidths = c("50%", "50%"), 
                                                   plotOutput("lin_reg"), 
                                                   gt_output("lin_reg_metrics")
                                                   )       
                                       ),
                              p("While the linear models consistently show a positive
                                relationship between expenditures and the percent of 
                                the vote earned by a Senate campaign, the metrics 
                                from these models suggest that there is a weak relationship,
                                with R squared being less than .3 in the models 
                                produced for each election cycle. The analysis model above,
                                which compares the predictions of the linear model 
                                of the data to the actual outcomes of the elections,
                                it can clearly be seen that there is a lot more 
                                variance in the actual outcomes than the predicted 
                                outcomes. On the other hand, the local regression 
                                shows an initially positive trend that eventually 
                                levels out. This model suggests a different relationship
                                in which races with higher expenditures result in 
                                more evenly divided vote percentages and slim 
                                victory margins. However, it's possible that other
                                variables influence this relationship, such as whether
                                a given election is for an open seat, or if it is
                                a contest between an incumbent and challengers.")
                              )),
                 )
             ),

# I then added a tab with my distribution of vote percentages for incumbents,
# challengers, and candidates filling open seats, as this relationship was alluded
# to in the previous tab

tabPanel("Breakdown by Candidate Types",
         fluidPage(
             titlePanel("Distribution of Votes for Different Types of Candidates"),
             sidebarLayout(
                 sidebarPanel(
                     helpText("Choose a year to see the distribution of votes for 
                     different types of candidates and the model of expenditures 
                     and vote percentage."),
                     
# For graphs with a year input, I chose to have a slider instead of a drop-down
                     
                     sliderTextInput("year",
                                     label = "Year:",
                                     choices = c("1980", "1982", "1984", 
                                                 "1986", "1988", "1990", 
                                                 "1992", "1994", "1996", 
                                                 "1998", "2000", "2002", 
                                                 "2004", "2006", "2008", 
                                                 "2010", "2012", "2014",
                                                 "2016", "2018"),
                                     selected = 1980
                     )
                 ),
                 mainPanel(fluidRow(plotOutput("candidate_status")),
                           p("The posterior distribution above only reaffirms
                             the idea that incumbents have an advantage over their
                             challengers in Senate elections, with the median of the 
                             distribution for incumbents falling above 50% in every 
                             election cycle from 1980-2018. Also, open seats appear 
                             to be more contested, with the median of the 
                             distribution falling between 40% and 50%. This 
                             suggests that factors such as the type of candidate 
                             should be taken into account in the linear model."),
                           h2("Revisiting the Linear Model"),
                           fluidRow(plotOutput("lin_reg_breakdown")),
                           p("Here, when the models are broken down by candidate
                             type, different relationships appear. The relationship 
                             with candidates vying for open seats is unstable, with the
                             slope of the regression model varying from cycle to cycle.
                             This could probably be accounted for by a lack of open seats
                             in Senate elections, as more often elections see challengers
                             going against incumbents. This lack of data also made it
                             difficult to measure the accuracy of the model, as there
                             were not enough data points to calculate a value for 
                             R squared, which was done with the previous model. 
                             However, across election cycles a trend could be seen 
                             for incumbents and challengers. Rather than there being
                             a solely positive trend, as suggested by the previous model,
                             there appears to be a negative trend between campaign
                             expenditures and election performance for incumbents,
                             and a positive relationship for challengers. This indicates
                             that as expenditures increase in an election between an 
                             incumbent and a challenger, the incumbent begins to lose the
                             advantage shown in the posterior distribution above, 
                             and the margin of victory decreases.")
                 )
             )
         )
),

# I then added my about tab, including links that related to the overall idea behind
# the project

    tabPanel("About",
             titlePanel("About"),
             h3("Background"),
             p("This project aims to analyze the the role that campaign finance
             plays in the performance of Senate campaigns, while taking into 
             consideration other variables that can influence outcomes. 
               It was largely motivated by the recent increase in campaign 
               contributions and expenditures in highly contested Senate races, such as
               the",
               a(href = "https://apnews.com/article/senate-elections-jaime-harrison
               -campaigns-lindsey-graham-south-carolina-8e132424f3f468c8aaf9806ba2c2c965",
                 "2020 South Carolina race"),  
                "between Jaime Harrison and Lindsey Graham,
               in which Harrison raised $30 million dollars more than Graham, but lost 
               by a margin of 11 percentage points. Even now, campaign finance records
               are set to be broken again with the upcoming runoff elections in Georgia,
               where the campaigns of each of the candidates continue to be",
               a(href = "https://fivethirtyeight.com/features/where-are-georgias
               -senate-candidates-getting-all-that-cash-from/", "raising millions of
               dollars"), "from donors across the country, with the two Democratic
               campaigns currently raising more than their Republican opponents.
               This project analyzes previous Senate elections to see if raising 
               and spending more money is related to the overall support for candidates,
               especially in scenarios where a challenger is going against an 
               incumbent, which is the case in many elections that have seen massive
               amounts of spending, such as the aforementioned races in South 
               Carolina and Georgia."),
             h3("Data"),
             p("The campaign finance data used for this project was collected from the ", 
               a(href = "https://www.fec.gov/data/candidates/senate/?election_ye
                 ar=2018&cycle=2018&election_full=true&is_active_candidate=true",
               "Federal Election Commission"), "for each election cycle from 
               1980 - 2018 and the election outcome data was collected from the", 
               a(href = "https://dataverse.harvard.edu/dataset.xhtml?persistentId
                 =doi:10.7910/DVN/PEJ5QU",
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
            ggplot(aes(x = year, y = diff/1000, color = party)) +
            geom_point(size = 5) +
            
# I changed the color scheme of the graph to have the points also reflect the
# winning party
            
            scale_color_manual(breaks = c("DEM", "REP", "IND"),
                values = c("DEM" = "blue", "REP" = "red", "IND" = "green"), 
                               labels = c("Democrat", "Republican", "Independent")) +
            labs(title = "How Much Winning Campaigns Outspent Opponents",
                x = "Election Year",
                y = "Spending Margin (in Millions of Dollars)",
                color = "Party of Winning Candidate") +
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
            scale_x_continuous(labels = scales::percent_format(scale = 1, 
                                                               accuracy = 1L)) +
            scale_y_continuous(labels = scales::percent_format(accuracy = 1L)) +
            scale_fill_manual(breaks = c("challenger", "incumbent", "open_seat"),
                                values = c("challenger" = "red", "incumbent" = 
                                               "blue", "open_seat" = "#FFCC33"), 
                                labels = c("Challenger", "Incumbent", "Open Seat")) +
            theme(text = element_text(size = 15), 
                  axis.text = element_text(size = 15), 
                  axis.title = element_text(size = 15))
        
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
        
        votes_wfl <- workflow() %>% 
            add_model(linear_reg() %>%
                          set_engine("stan") %>%
                          set_mode("regression")) %>% 
            add_recipe(recipe(vote_percentage ~  disbursements,
                              
# The data was filtered based on the input
                              
                              data = votes_train %>% filter(year == input_plot)) %>% 
                           step_dummy(all_nominal())) %>% 
            fit(data = votes_train %>% filter(year == input_plot)) %>% 
            predict(new_data = votes_test %>% filter(year == input_plot)) %>% 
            bind_cols(votes_train %>% filter(year == input_plot) %>% select(vote_percentage)) 
        votes_wfl %>% 
            ggplot(aes(x = vote_percentage, y = .pred)) +
            geom_point(size = 3) +
            theme_bw() +
            
# Again, the code from the .rmd was used and appropriate aesthetics were added
            
            labs(x = "Actual Percent of Vote Received",
                 y = "Predicted Percent of Vote Received") +
            theme(text = element_text(size = 15), 
                  axis.text = element_text(size = 15), 
                  axis.title = element_text(size = 15),
                  plot.title = element_text(hjust = .5, face = "bold")) +
            scale_x_continuous(labels = scales::percent_format(scale = 1,
                                                               accuracy = 1L)) +
            scale_y_continuous(labels = scales::percent_format(scale = 1,
                                                               accuracy = 1L))
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
            labs(title = "Linear Model",
                 x = "Expenditures (in Thousands of Dollars)",
                 y = "Percent of Vote Received") +
            theme(text = element_text(size = 15), 
                  axis.text = element_text(size = 15), 
                  axis.title = element_text(size = 15),
                  plot.title = element_text(hjust = .5)) +
            scale_y_continuous(labels = scales::percent_format(scale = 1, 
                                                               accuracy = 1L)) +
            scale_x_continuous(labels = scales::comma_format())
    })
    output$lin_reg_metrics <- render_gt(
        
# For my rmse table, I manually made the training dataset inside the render_gt()
# function
        
        votes_wfl <- workflow() %>% 
            add_model(linear_reg() %>%
                          set_engine("stan") %>%
                          set_mode("regression")) %>% 
            add_recipe(recipe(vote_percentage ~  disbursements,
                              data = votes_train %>% 
                                  filter(year == input$year_2)) %>% 
                           step_dummy(all_nominal())) %>% 
            fit_resamples(resamples = vfold_cv(votes_train %>% 
                                                   filter(year == input$year_2), 
                                               v = 10)) %>% 
            collect_metrics() %>% 
            rename("Metric" = .metric,
                   "Mean" = mean,
                   "Estimator" = .estimator,
                   "Standard Error" = std_err) %>% 
            gt()
    )
    
# I decided to include the local regression since it shows how the trend appears
# to change as expenditures increase
    
    output$loess <- renderPlot({
        votes %>% 
            filter(year == input$year_2) %>% 
            ggplot(aes(x = disbursements, y = vote_percentage)) +
            geom_point() +
            geom_smooth(formula = y ~ x, 
                        method = "loess", 
                        color = "red",
                        se = FALSE) +
            theme_classic() +
            labs(title = "Local Regression Model",
                 x = "Expenditures (in Thousands of Dollars)",
                 y = "Percent of Vote Received") +
            
# I used my code from the .rmd, switching out the example filter for the input 
# from the slider and adding additional aesthetics such as labels and larger
# axis text
            
            theme(text = element_text(size = 15), 
                  axis.text = element_text(size = 15), 
                  axis.title = element_text(size = 15),
                  plot.title = element_text(hjust = .5)) +
            scale_y_continuous(labels = scales::percent_format(scale = 1, 
                                                               accuracy = 1L)) +
            scale_x_continuous(labels = scales::comma_format())
    })
    
# For the updated linear model, I used the code from the .rmd, switching out
# the example 1980 for input$year, since I wanted all the graphs on the page 
# to update with the slider input
    
    output$lin_reg_breakdown <- renderPlot({
        
        challenger <- contribution_votes %>% 
            filter(year == input$year) %>% 
            filter(incumbent_challenge_full == "Challenger") %>% 
            drop_na()
        
        incumbent <- contribution_votes %>% 
            filter(year == input$year) %>% 
            filter(incumbent_challenge_full == "Incumbent") %>% 
            drop_na()
        
        open_seat <- contribution_votes %>% 
            filter(year == input$year) %>% 
            filter(incumbent_challenge_full == "Open seat") %>% 
            drop_na()
        
        model_4 <- stan_glm(data = incumbent,
                            vote_percentage ~ disbursements,
                            family = gaussian(),
                            refresh = 0)
        
        model_5 <- stan_glm(data = challenger,
                            vote_percentage ~ disbursements,
                            family = gaussian(),
                            refresh = 0)
        
        model_6 <- stan_glm(data = open_seat,
                            vote_percentage ~ disbursements,
                            family = gaussian(),
                            refresh = 0)
        
        incumbent %>% 
            ggplot(aes(x = disbursements, 
                       y = vote_percentage)) +
            geom_point(aes(color = "blue")) +
            geom_line(aes(y = fitted(model_4), 
                          color = "blue")) +
            geom_point(data = challenger, 
                       aes(x = disbursements, 
                           y = vote_percentage, 
                           color = "red")) +
            geom_line(data = challenger, 
                      aes(y = fitted(model_5),
                          color = "red")) +
            geom_point(data = open_seat,
                       aes(x = disbursements, 
                           y = vote_percentage,
                           color = "yellow")) +
            geom_line(data = open_seat, 
                      aes(y = fitted(model_6), 
                          color = "yellow")) +
            scale_color_manual(breaks = c("red", "blue", "yellow"),
                               values = c("blue" = "blue", 
                                          "yellow" = "#FFCC33",
                                          "red" = "red"),
                               labels = c("Challenger", "Incumbent", "Open Seat")) +
            theme_classic() +
            
# Again, I added appropriate labels and scales
            
            theme(text = element_text(size = 15), 
                  axis.text = element_text(size = 15), 
                  axis.title = element_text(size = 15)) +
            labs(x = "Expenditures (in Thousands of Dollars)",
                 y = "Percent of Vote Received",
                 color = "Type of Candidate") +
            scale_y_continuous(labels = scales::percent_format(scale = 1, 
                                                               accuracy = 1L)) +
            scale_x_continuous(labels = scales::comma_format())
            
            
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
