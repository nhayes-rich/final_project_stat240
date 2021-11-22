library(leaflet)
library(stringr)
library(RColorBrewer)
library(tidyverse)
library(caret)
library(forcats)
library(maps)
library(openintro)
library(usdata)
library(shiny)

### --- DATA IMPORT --- ###

map_data_full <- read_csv("map_data_full.csv")
raising_by_employer <- read_csv("raising_by_employer.csv")
raising_by_occupation <- read_csv("raising_by_occupation.csv")
raising_full <- read_csv("raising_full.csv")
spending_full <- read_csv("spending_full.csv")
spending_by_desc <- read_csv("spending_by_desc.csv")
spending_by_recipient_name <- read_csv("spending_by_recipient_name.csv")

### --- MAP CREATION --- ###

usa_map <- maps::map("state", fill = TRUE, plot = FALSE)

# The map contains different sections for a single state, labelled differently
# i.e. Washington:main, or New York:manhattan
# Leaflet requires all of these sections to plot the map correctly
# So, we just modify the "names" section to have a bunch of duplicates,
# that way, when we add in the other data, like votes / etc, users
# will always see the correct data over any part of a state
# and will also see "Washington" instead of "Washington:main"
# We also do one final correction to make District of Columbia match
# between datasets`

usa_map$names <- str_replace_all(str_to_title(str_remove_all(usa_map$names, ":.+")), "Of", "of")
names_df <- tibble(state = usa_map$names) 

### --- MODEL DATA IMPORT --- ###

train_2016_full_alt <- read_csv("train_2016_full_alt.csv")
predict_2020_alt <- read_csv("predict_2020_alt.csv")
test_2020_full_alt <- read_csv("test_2020_full_alt.csv")

### --- SPENDING WRANGLING --- ###

spending_df <- names_df %>% left_join(spending_full  %>%
                                          mutate(state = abbr2state(recipient_st)) %>%
                                          group_by(state) %>%
                                          summarize(disb_amt = sum(disb_amt, na.rm = TRUE), n = sum(n, na.rm = TRUE)))

pal_spending <- colorNumeric(
    palette = "RdYlGn",
    domain = spending_df %>% pull(disb_amt)
)



### -------------------- SHINY APP ------------------------------ ###

ui <- fluidPage(theme = "bootstrap.css",
                tabsetPanel(
                    tabPanel("Introduction", 
                             includeHTML("html/introduction.html")),
                    navbarMenu("The Data", 
                               tabPanel("Voter Data",
                                        includeHTML("html/voter_data.html"),
                                        dataTableOutput("voter_data")),
                               tabPanel("Spending Data",
                                        includeHTML("html/spending_data.html"),
                                        dataTableOutput("spending_data")),
                               tabPanel("Fundraising Data",
                                        includeHTML("html/fundraising_data.html"),
                                        dataTableOutput("fundraising_data"))),
                    tabPanel("Mapping it Out",
                             tabsetPanel(
                                 tabPanel("Description", 
                                          includeHTML("html/general_map_description.html")),
                                 tabPanel("Map",
                                          leafletOutput("general_map"),
                                          selectInput("general_year", "Year", c("2016", "2020")),
                                          varSelectInput("general_variable", "Display Data", map_data_full %>% select(Spending, Raising, `Dollar Spent per Vote`, `Dollar Raised per Vote`, Votes)),
                                          selectInput("general_candidate", "Candidate", c("Clinton", "Trump")),
                                          checkboxInput("general_include_dc", "Include DC in Color Map?"),
                                          br(),
                                          h1("More Detailed Breakdown of the Data"),
                                          hr(),
                                          dataTableOutput("general_map_data")))
                    ),
                    navbarMenu("Fundraising Trends",
                               tabPanel("Donations by Party",
                                        includeHTML("html/fundraising_by_party.html"),
                                        leafletOutput("donation_map"),
                                        selectInput("donation_year", "Year", c("2016", "2020")),
                                        varSelectInput("donation_variable", "Display Data", data = tibble(Donation = c(),
                                                                                                          Count = c(),
                                                                                                          `Average Donation` = c()))
                               ),
                               tabPanel("Donation Breakdown by Employer",
                                        includeHTML("html/fundraising_by_employer.html"),
                                        sidebarLayout(mainPanel = 
                                                          mainPanel(
                                                              plotOutput("donation_by_employer")
                                                          ),
                                                      sidebarPanel = 
                                                          sidebarPanel(
                                                              selectInput("donation_by_employer_year", "Year", c("2016", "2020")),
                                                              varSelectInput("donation_by_employer_variable", "Display Data", data = tibble(Donation = c(),
                                                                                                                                            Count = c(),
                                                                                                                                            `Average Donation` = c())),
                                                              selectInput("donation_by_employer_candidate", "Candidate", c("Clinton", "Trump"))
                                                          )
                                        ),
                                        br(),
                                        h1("More Detailed Breakdown of Donations by Employer"),
                                        hr(),
                                        dataTableOutput("donation_by_employer_table")
                               ),
                               tabPanel("Donation Breakdown by Occupation",
                                        includeHTML("html/fundraising_by_occupation.html"),
                                        sidebarLayout(mainPanel = 
                                                          mainPanel(
                                                              plotOutput("donation_by_occupation")
                                                          ),
                                                      sidebarPanel = 
                                                          sidebarPanel(
                                                              selectInput("donation_by_occupation_year", "Year", c("2016", "2020")),
                                                              varSelectInput("donation_by_occupation_variable", "Display Data", data = tibble(Donation = c(),
                                                                                                                                              Count = c(),
                                                                                                                                              `Average Donation` = c())),
                                                              selectInput("donation_by_occupation_candidate", "Candidate", c("Clinton", "Trump"))
                                                          )
                                        ),
                                        br(),
                                        h1("More Detailed Breakdown of Donations by Occupation"),
                                        hr(),
                                        dataTableOutput("donation_by_occupation_table")
                               )
                    ),
                    navbarMenu("Spending Trends",
                               tabPanel("Spending Breakdown by Party",
                                        includeHTML("html/spending_by_party.html"),
                                        leafletOutput("spending_map"),
                                        selectInput("spending_year", "Year", c("2016", "2020")),
                                        varSelectInput(inputId = "spending_variable",
                                                       "Display Data",
                                                       data = tibble(Spending = c(), Count = c(), `Average Spending` = c()))),
                               tabPanel("Spending Breakdown by Purpose",
                                        includeHTML("html/spending_by_purpose.html"),
                                        sidebarLayout(mainPanel = 
                                                          mainPanel(plotOutput("spending_by_purpose")
                                                          ),
                                                      sidebarPanel = sidebarPanel(selectInput("spending_by_purpose_year", 
                                                                                              "Year", 
                                                                                              c("2016", "2020")),
                                                                                  varSelectInput("spending_by_purpose_variable",
                                                                                                 "Display Data",
                                                                                                 data = tibble(Spending = c(), Count = c(), `Average Spending` = c())),
                                                                                  selectInput("spending_by_purpose_candidate", "Candidate", c("Clinton", "Trump"))
                                                      )
                                        ),
                                        br(),
                                        h1("More Detailed Breakdown of Spending by Spending Purposes"),
                                        hr(),
                                        dataTableOutput("purpose_table")
                               ),
                               tabPanel("Spending Breakdown by Recipient",
                                        includeHTML("html/spending_by_recipient.html"),
                                        sidebarLayout(mainPanel = mainPanel(plotOutput("spending_by_recipient")),
                                                      sidebarPanel = sidebarPanel(
                                                          selectInput("spending_by_recipient_year", "Year", c("2016", "2020")),
                                                          varSelectInput("spending_by_recipient_variable",
                                                                         "Display Data",
                                                                         data = tibble(Spending = c(), Count = c(), `Average Spending` = c())),
                                                          selectInput("spending_by_recipient_candidate", "Candidate", c("Clinton", "Trump"))
                                                      )),
                                        br(),
                                        h1("More Detailed Breakdown of Spending by Recipient Names"),
                                        hr(),
                                        dataTableOutput("recipient_table")
                               )
                    ),
                    tabPanel("Prediction model", 
                             tabsetPanel(tabPanel("Description", 
                                                  includeHTML("html/prediction_model.html")),
                                         tabPanel("Prediction map",
                                                  leafletOutput("result_map"),
                                                  absolutePanel(
                                                      selectInput("mode", "Select the display", c("2016 election results", 
                                                                                                  "2020 predicted election results",
                                                                                                  "2020 election results"))))))
                    
                ) 
)
server <- function(input, output, session) {
    ### THE DATA
    output$voter_data <- renderDataTable({map_data_full %>% select(state, candidate, party, votes, total_votes, pct, Won)})
    output$spending_data <- renderDataTable({spending_full})
    output$fundraising_data <- renderDataTable({raising_full})
    
    ### MAPPING IT OUT
    output$general_map_data <- renderDataTable({
        map_data_full %>% 
            filter(year == input$general_year, candidate == input$general_candidate) %>%
            select(state, candidate, !!input$general_variable)
    })
    
    general_map_data <- reactive({
        map_data_full %>% 
            filter(year == input$general_year, candidate == input$general_candidate) %>%
            select(state, candidate, !!input$general_variable) %>%
            right_join(names_df, by = c("state")) %>%
            pull(!!input$general_variable)
    })
    general_pal_reactive <- reactive({
        data <- log(general_map_data())
        if (input$general_include_dc) {
            domain <- data 
        } else {
            domain <- data[-8] # remove DC from the domain so it doesn't affect the coloring of other state
            # gives us error in coloring map, but allows visualization of others without huge outlier
        }
        
        pal <- colorNumeric(
            palette = "RdYlGn",
            domain = domain
        )
        
        pal(data)
    })
    output$general_map <- renderLeaflet({
        req(general_map_data())
        values <- general_map_data()
        dollar_sign <- ifelse(input$general_variable == "Votes", "", "$")
        format_num <- ifelse(input$general_variable == "Votes", "d", "f")
        leaflet(data = usa_map) %>%
            addTiles() %>%
            addPolygons(
                label=~lapply(
                    str_c("<b>", names, "</b> <br>", dollar_sign,
                          formatC(values, big.mark = ",", format = format_num, digits = 2)),
                    htmltools::HTML),
                fillColor = ~general_pal_reactive(), 
                stroke = FALSE) %>%
            addMarkers(
                lng = -77.0369, lat = 38.9072, label = htmltools::HTML(str_c("<b>", "District of Columbia", "</b><br>", 
                                                                             dollar_sign, 
                                                                             formatC(values[[8]], big.mark = ",", # number 8 is district of columbia
                                                                                     format = format_num, 
                                                                                     digits = 2))) 
            )
    })
    # Change candidate options
    candidate_opt <- reactive({
        if (input$general_year == "2016") {
            c("Clinton", "Trump")
        } else {
            c("Biden", "Trump")
        }
    })
    observe({
        updateSelectInput(session, "general_candidate", choices = candidate_opt())
    })
    
    ### FUNDRAISING TRENDS
    donation_map_data <- reactive({
        raising_split_party <- raising_full %>% 
            filter(year == input$donation_year) %>%
            group_by(state, party) %>%
            summarize(contb_receipt_amt = sum(contb_receipt_amt), n = sum(n)) %>%
            mutate(Donation = contb_receipt_amt, Count = n, `Average Donation` = Donation / Count) %>%
            select(state, party, !!input$donation_variable) %>%
            pivot_wider(names_from = party,
                        values_from = c(!!input$donation_variable)) %>%
            right_join(names_df, by = c("state")) %>%
            mutate(difference = DEM - REP) %>%
            select(DEM, REP, difference)
    })
    donation_pal_reactive <- reactive({
        data <- donation_map_data() %>% pull(difference)
        
        # We split the data into two groups: dems ahead, reps ahead
        # This produces some warnings for when data is outside of the domain
        # but gives a more intuitive color map (i.e. dems are blue, reps are red), 
        # since leaflet doesn't allow specification of a central value
        # not doing this means that even in situations where dems are ahead,
        # the state can end up being colored red (and vice versa)
        
        rep_indices <- which(data < 0)
        dem_indices <- which(data >= 0)
        
        pal_reds <- colorNumeric(
            palette = "Reds",
            domain = log(abs(data[which(data < 0)]))
        )
        
        pal_blues <- colorNumeric(
            palette = "Blues",
            domain = log(data[which(data >= 0)])
        )
        
        ifelse(data < 0, pal_reds(log(abs(data))), pal_blues(log(abs(data))))
    })
    output$donation_map <- renderLeaflet({
        req(donation_map_data())
        dem_values <- donation_map_data() %>% pull(DEM)
        rep_values <- donation_map_data() %>% pull(REP)
        dollar_sign <- ifelse(input$donation_variable == "Count", "", "$")
        format_num <- ifelse(input$donation_variable == "Count", "d", "f")
        leaflet(data = usa_map) %>%
            addTiles() %>%
            addPolygons(
                label=~lapply(
                    str_c("<b>", names, "</b> <br>", 
                          "Democrats: ", dollar_sign, formatC(dem_values, big.mark = ",", format = format_num, digits = 2), "<br>",
                          "Republicans: ", dollar_sign, formatC(rep_values, big.mark = ",", format = format_num, digits = 2)),
                    htmltools::HTML),
                fillColor = ~donation_pal_reactive(), 
                stroke = FALSE) %>%
            addMarkers(
                lng = -77.0369, lat = 38.9072, label = htmltools::HTML(str_c("<b>", "District of Columbia", "</b> <br>", 
                                                                             "Democrats: ", dollar_sign, formatC(dem_values[[8]], big.mark = ",", format = format_num, digits = 2), "<br>",
                                                                             "Republicans: ", dollar_sign, formatC(rep_values[[8]], big.mark = ",", format = format_num, digits = 2)))
            )
    })
    
    donation_employer_data <- reactive({
        if (input$donation_by_employer_variable == "Average Donation") {
            raising_by_employer$avg_don_logical <- raising_by_employer$n > 100 # only get n > 100 donations for average donation classifier
        } else {
            raising_by_employer$avg_don_logical <- rep(TRUE, nrow(raising_by_employer))
        }
        
        raising_by_employer %>%
            ungroup() %>%
            filter(year == input$donation_by_employer_year, candidate == input$donation_by_employer_candidate, !is.na(contbr_employer), avg_don_logical) %>%
            mutate(Donation = contb_receipt_amt, Count = n, `Average Donation` = Donation / Count) %>%
            arrange(desc(!!input$donation_by_employer_variable))
    })
    
    output$donation_by_employer <- renderPlot({
        donation_employer_data() %>%
            slice_max(order_by = !!input$donation_by_employer_variable, n = 15) %>%
            ggplot() +
            geom_bar(aes(x = reorder(contbr_employer, !!input$donation_by_employer_variable), y = !!input$donation_by_employer_variable), stat = "identity") + 
            coord_flip() + 
            scale_y_log10() +
            labs(x = "15 Most Common Employers", y = str_c("Log Scale ", input$donation_by_employer_variable))
    })
    
    output$donation_by_employer_table <- renderDataTable({
        donation_employer_data() %>% 
            select(-year, -candidate, -contb_receipt_amt, -n, -avg_don_logical)
    })
    
    donation_occupation_data <- reactive({
        if (input$donation_by_occupation_variable == "Average Donation") {
            raising_by_occupation$avg_don_logical <- raising_by_occupation$n > 100 # only get n > 100 donations for average donation classifier
        } else {
            raising_by_occupation$avg_don_logical <- rep(TRUE, nrow(raising_by_occupation))
        }
        
        raising_by_occupation %>%
            ungroup() %>%
            filter(year == input$donation_by_occupation_year, candidate == input$donation_by_occupation_candidate, !is.na(contbr_occupation), avg_don_logical) %>%
            mutate(Donation = contb_receipt_amt, Count = n, `Average Donation` = Donation / Count) %>%
            arrange(desc(!!input$donation_by_occupation_variable))
    })
    
    output$donation_by_occupation <- renderPlot({
        donation_occupation_data() %>%
            slice_max(order_by = !!input$donation_by_occupation_variable, n = 15) %>%
            ggplot() +
            geom_bar(aes(x = reorder(contbr_occupation, !!input$donation_by_occupation_variable), y = !!input$donation_by_occupation_variable), stat = "identity") + 
            coord_flip() + 
            scale_y_log10() +
            labs(x = "15 Most Common Occupations", y = str_c("Log Scale ", input$donation_by_occupation_variable))
    })
    
    output$donation_by_occupation_table <- renderDataTable({
        donation_occupation_data()  %>% 
            select(-year, -candidate, -contb_receipt_amt, -n, -avg_don_logical)
    })
    
    employer_candidate_opt <- reactive({
        if (input$donation_by_employer_year == "2016") {
            c("Clinton", "Trump")
        } else {
            c("Biden", "Trump")
        }
    })
    observe({
        updateSelectInput(session, "donation_by_employer_candidate", choices = employer_candidate_opt())
    })
    
    occupation_candidate_opt <- reactive({
        if (input$donation_by_occupation_year == "2016") {
            c("Clinton", "Trump")
        } else {
            c("Biden", "Trump")
        }
    })
    observe({
        updateSelectInput(session, "donation_by_occupation_candidate", choices = occupation_candidate_opt())
    })
    
    ### SPENDING TRENDS
    spending_map_data <- reactive({
        spending_split_party <- spending_full %>%
            filter(year == input$spending_year) %>%
            group_by(state, party) %>%
            summarize(disb_amt = sum(disb_amt), n = sum(n)) %>%
            mutate(Spending = disb_amt, Count = n, `Average Spending` = Spending / Count) %>%
            select(state, party, !!input$spending_variable) %>%
            pivot_wider(names_from = party,
                        values_from = c(!!input$spending_variable)) %>%
            right_join(names_df, by = c("state")) %>%
            mutate(difference = DEM - REP) %>%
            select(DEM, REP, difference)
    })
    spending_pal_reactive <- reactive ({
        data <- spending_map_data() %>% pull(difference)
        rep_indices <- which(data < 0)
        dem_indices <- which(data >= 0)
        
        pal_reds <- colorNumeric(
            palette = "Reds",
            domain = log(abs(data[which(data < 0)]))
        )
        
        pal_blues <- colorNumeric(
            palette = "Blues",
            domain = log(data[which(data >= 0)])
        )
        
        ifelse(data < 0, pal_reds(log(abs(data))), pal_blues(log(abs(data))))
        
    })
    output$spending_map <- renderLeaflet({
        req(spending_map_data())
        dem_values <- spending_map_data() %>% pull(DEM)
        rep_values <- spending_map_data() %>% pull(REP)
        dollar_sign <- ifelse(input$spending_variable == "Count", "", "$")
        format_num <- ifelse(input$spending_variable == "Count", "d", "f")
        leaflet(data = usa_map) %>%
            addTiles() %>%
            addPolygons(
                label=~lapply(
                    str_c("<b>", names, "</b> <br>", 
                          "Democrats: ", dollar_sign, formatC(dem_values, big.mark = ",", format = format_num, digits = 2), "<br>",
                          "Republicans: ", dollar_sign, formatC(rep_values, big.mark = ",", format = format_num, digits = 2)),
                    htmltools::HTML),
                fillColor = ~spending_pal_reactive(), 
                stroke = FALSE) %>%
            addMarkers(
                lng = -77.0369, lat = 38.9072, label = htmltools::HTML(str_c("<b>", "District of Columbia", "</b> <br>", 
                                                                             "Democrats: ", 
                                                                             dollar_sign, 
                                                                             formatC(dem_values[[8]],
                                                                                     big.mark = ",",
                                                                                     format = format_num, digits = 2), "<br>",
                                                                             "Republicans: ", dollar_sign,
                                                                             formatC(rep_values[[8]],
                                                                                     big.mark = ",",
                                                                                     format = format_num,
                                                                                     digits = 2)))
            )
    })
    spending_purpose_data <- reactive ({
        if (input$spending_by_purpose_variable == "Average Spenging") {
            spending_by_desc$avg_spending_logical <- spending_by_desc$n > 100
        } else {
            spending_by_desc$avg_spending_logical <- rep(TRUE, nrow(spending_by_desc))
        }
        
        spending_by_desc %>%
            ungroup() %>%
            filter(year == input$spending_by_purpose_year, candidate == input$spending_by_purpose_candidate, !is.na(disb_desc), 
                   avg_spending_logical) %>%
            mutate(Spending = disb_amt, Count = n, `Average Spending` = Spending / Count) %>%
            arrange(desc(!!input$spending_by_purpose_variable))
        
    })
    output$spending_by_purpose <- renderPlot({
        spending_purpose_data() %>%
            ungroup() %>%
            slice_max(order_by = !!input$spending_by_purpose_variable, n = 15 ) %>%
            ggplot() +
            geom_bar(aes(x = reorder(disb_desc, !!input$spending_by_purpose_variable),
                         y = !!input$spending_by_purpose_variable), stat = "identity") + 
            coord_flip() + 
            scale_y_log10() +
            labs(x = "15 Most Common Purposes", y = str_c("Log Scale ", input$spending_by_purpose_variable))
    })
    output$purpose_table <- renderDataTable({ #it was output$spending_purpose_table
        spending_purpose_data() %>% 
            select(-year, -candidate, -disb_amt, -n, -avg_spending_logical)
    })
    
    
    spending_recipient_data <- reactive ({
        if (input$spending_by_recipient_variable == "Average Spenging") {
            spending_by_recipient_name$avg_spending_logical <- spending_by_recipient_name$n > 100
        } else {
            spending_by_recipient_name$avg_spending_logical <- rep(TRUE, nrow(spending_by_recipient_name))
        }
        
        spending_by_recipient_name %>%
            ungroup() %>%
            filter(year == input$spending_by_recipient_year, candidate == input$spending_by_recipient_candidate, avg_spending_logical,
                   !is.na(recipient_nm)) %>%
            mutate(Spending = disb_amt, Count = n, `Average Spending` = Spending / Count) %>%
            arrange(desc(!!input$spending_by_recipient_variable))
        
    })
    output$spending_by_recipient <- renderPlot({
        spending_recipient_data() %>%
            ungroup() %>%
            slice_max(order_by = !!input$spending_by_recipient_variable, n = 15 ) %>%
            ggplot() +
            geom_bar(aes(x = reorder(recipient_nm, !!input$spending_by_recipient_variable),
                         y = !!input$spending_by_recipient_variable), stat = "identity") + 
            coord_flip() + 
            scale_y_log10() +
            labs(x = "15 Most Common Recipient Names", y = str_c("Log Scale ", input$spending_by_recipient_variable))
    })
    output$recipient_table <- renderDataTable({
        spending_recipient_data() %>% 
            select(-year, -candidate, -disb_amt, -n, -avg_spending_logical)
    })
    output$donation_by_occupation_table <- renderDataTable({
        donation_occupation_data()  %>% 
            select(-year, -candidate, -contb_receipt_amt, -n, -avg_don_logical)
    })
    
    purpose_candidate_opt <- reactive({
        if (input$spending_by_purpose_year == "2016") {
            c("Clinton", "Trump")
        } else {
            c("Biden", "Trump")
        }
    })
    observe({
        updateSelectInput(session, "spending_by_purpose_candidate", choices = purpose_candidate_opt())
    })
    
    recipient_candidate_opt <- reactive({
        if (input$spending_by_recipient_year == "2016") {
            c("Clinton", "Trump")
        } else {
            c("Biden", "Trump")
        }
    })
    observe({
        updateSelectInput(session, "spending_by_recipient_candidate", choices = recipient_candidate_opt())
    })
    
    
    
    ### PREDICTION
    prediction_data <- reactive({
        if (input$mode == "2016 election results") {
            data <- train_2016_full_alt
        } else if (input$mode == "2020 predicted election results") {
            data <- predict_2020_alt
        } else {
            data <- test_2020_full_alt
        }
        data <- data %>% select(state, won)
        data$won <- as.character(data$won)
        names_df %>% left_join(data, by = c("state")) %>% pull(won)
    })
    
    output$result_map <- renderLeaflet({
        leaflet(data = usa_map) %>%
            addTiles() %>%
            addPolygons(
                label=~lapply(
                    str_c("<b>", names, "</b> <br>", 
                          ifelse(prediction_data(), "Democratic", "Republican")),
                    htmltools::HTML),
                fillColor = ifelse(prediction_data(), "#0892d0", "#FF6666"), 
                stroke = FALSE)
    })
    
}
# Run the application 
shinyApp(ui = ui, server = server)
