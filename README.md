# final-project-shaun-david-nathan

## ShinyApp: https://hayesrichn.shinyapps.io/final-project-shaun-david-nathan/

## Original Datasets At: https://www.dropbox.com/sh/m3d6g0qj0ys16ic/AACNoMwV1j-WFYDD6ITOTgr4a?dl=0
 
## File Structure
model-prediction.Rmd - In this file, we wrangle the data into test, training, and prediction sets and then run our KNN model.   
data-wrangling.Rmd - In this file, we wrangle our data into the necessary formats for our shinyapp and then write those files to .csv. We also write the intermediate steps (as the original datasets are quite large, so we didn't want to lose any work along the way).   
app.R - This file contains our data wrangling, UI function and server function for our shiny app.   
html - This folder contains the HTML used to display descriptions / introductions to our data analysis. All of this content is present in the shiny app itself.   

### The following files are the original datasets in dropbox
spending-2016.csv - The FEC spending data for 2016.  
spending-2020.csv - The FEC spending data for 2020.    
raising-2016.csv - The FEC raising data for 2016.    
raising-2020.csv - The FEC raising data for 2020.  
pres2020_county_candidate.csv - Voter data by county and candidate for 2020.  
pres2020_county.csv - Voter data by county for 2020.  
pres2016results.csv - Voter data for 2000 - 2016 elections. This was the only dataset we could find that did not overcomplicate the 2016 election data.  

### The following files are the final datasets used in our shinyapp, created from data-wrangling.Rmd. Several of these datasets are still too large to upload to Github, so for consistency, we removed all of them.
train_2016_full_alt.csv - The 2016 training data used in model prediction.  
predict_2020_alt.csv - The 2020 predicted results.  
test_2020_full_alt.csv - The 2020 test data set used in model prediction.  
map_data_full.csv - The combined and reduced dataset used in presenting state by state values for spending, raising, and votes.  
raising_by_employer.csv - The fundraising dataset grouped and summarized by employer, state, year.  
raising_by_occupation.csv - The fundraising dataset grouped and summarized by occupation, state, year.  
spending_full.csv - The combined 2016 and 2020 spending dataset.  
spending_by_desc.csv - The spending dataset grouped and summarized by description, state, year.  
spending_by_recipient_name.csv - The spending dataset grouped and summarized by recipient name, state, year.  
raising_full.csv - The combined 2016 and 2020 fundraising dataset.

## Website/App Creation
Our website creation is broken down into the following (labelled in app.R) sections:  
LOAD ORIGINAL DATA / WRITE REDUCED DATA: Here we load the original datasets and write them into their reduced forms.  
DATA IMPORT: Here we read the data from disk and make some necessary mutations to factor variables (such as candidate names, party names, etc.)  
DATA WRANGLING FOR MAP: Here we wrangle all the data into a format usable for our second tab containing the introductory map. We use all of the data in the introductory map so it’s important to combine everything correctly.  
FUNDRAISING WRANGLING: Here we wrangle the fundraising data into smaller blocks (like raising_by_employer and raising_by_occupation) so that we can use them for analysis in the fundraising tab. We break it into smaller blocks here because the datasets are still too large to work with in their entirety on the fly.  
MODEL WRANGLING: Here we create training and test datasets for our KNN model. At the bottom of the section we also run our KNN model and store the prediction results.  
SPENDING WRANGLING: Same as fundraising wrangling, but for the FEC spending data.  
SHINY APP: Here we create the UI and fill in the output!

## Class Tools/Methods
To import data into RStudio, we use functions from the readr package like read_csv() and from the readxl package like read_xlsx().   
We use data wrangling functions from the dyplyr package like group_by(), summarize(), filter(), mutate(),  join(), … Furthermore, some of the base R functions are also used to extract and manipulate data. We create several functions to help with this process.   
To conduct the exploratory data analysis and some of the graphics in the application, we also use visualization tools from the ggplot2 package. We also use some color patterns for some of our graphics in the RColorBrewer package. The maps package helps us map our data to an actual map of the United States.  
We use the caret package to fit statistical learning algorithms to our data like the K-NN algorithm, binary logistic regression, random forest, cross validation technique. Concepts from this topic like training set and test set are also applied using the 2016 and 2020 election data.  
We use the template and functions from the shiny package to create a shiny application using the tools discussed in class. We deploy our final application in the server shinyapps.io 

## New Tools/Methods
One of the main components of this website is the map that displays fundraising for each candidate by state, spending by each candidate by state, and the votes each candidate received by state. To create this, we used the package leaflet. Since the leaflet map can have multiple sections/locations for a single state, some states may have more than one label, like New York::manhattan. Since leaflet requires all these sections/locations to plot a map correctly, the data frames for fundraising, spending, and prediction had to be joined by state in a way to match every row of those data frames with the map data frame by state and location.
