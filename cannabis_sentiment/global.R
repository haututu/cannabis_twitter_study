library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(dplyr)
library(ggplot2)
library(plotly)
library(wesanderson)
library(shinycssloaders)

dat <- readr::read_csv("cannabis_data.csv") %>%
  na.omit()

text_insight <- "Over the selected period, XX% of people were pro-legalisation and XX% were anti-legalisation."

text_background <- "
In September 2020, New Zealand will vote on whether cannabis should be legalised for recreational use. Given the popularity of social media sites and how common cannabis is referenced online, our aim is to report how New Zealanders are talking about cannabis on Twitter as the referendum approaches (in close to real time).

Enrol to vote: https://www.govt.nz/browse/engaging-with-government/enrol-and-vote-in-an-election/enrol-to-vote/
Read up on what you are voting on: https://www.referendums.govt.nz/cannabis/summary.html
"

text_method <- "
We used a Twitter-sponsored commercial platform to access all historic Tweets written in New Zealand that reference cannabis (around 350,000). We then used the platform’s machine learning function to classify the sentiment of each of the Tweets (pro-cannabis, anti-cannabis, or neutral). 
Pro cannabis Tweets reference: intention to use, previous use, current use, cravings, medicinal benefits, recreational benefits (feelings, facilitating friendships, stress relief/to relax), lack of harms of cannabis, encourage others to use, or an intention to “Vote yes”. 
Anti-cannabis Tweets reference: cannabis users negatively (unattractive, uncool, lazy), cannabis as harmful (causes psychosis), intention to not use, a negative experience, criminal impacts of cannabis, or an intention to “Vote No”.
Neutral Tweets are neither pro- nor anti-cannabis and are typically news stories.
"

text_how_to <- "
Use the buttons on the left to choose whether to look at all cannabis-related Tweets or only referendum-related Tweets. You can also select whether to include reTweets, the date range, and whether the results are presented as a proportion or counts.
We recommend that you focus on recent Tweets (e.g., Tweets since the last election: September, 2017).
"

text_about_us <- "
We are researchers from the University of Otago (Damian Scarf), University of Sydney, (Benjamin Riordan, Jacques Raubenheimer), Brown University (Jennifer Merrill), Miami University (Rose Marie Ward), and University of Victoria Wellington (Taylor Winter). Our group’s research broadly focuses on psychology, health, and substance use.
Follow some of our other research at: https://www.facebook.com/theabcdlab/
We are still working on the application and we will publish a preprint soon. For updates, please get in touch with Benjamin Riordan at Benjamin.riordan@sydney.edu.au or Damian Scarf at damian.scarf@otago.ac.nz.
"