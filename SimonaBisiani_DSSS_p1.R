# Simona Bisiani
# 27.04.2020
# War and Music - a scraping project from Wikipedia

# PACKAGES
library(RCurl) # for scraping
library(XML) # for scraping 
library(httr) # for scraping
library(tidyverse) # for data manipulation and visualization
library(rvest) # for scraping
library(htmltab) # for scraping (extracting tables)
library(lubridate) # to format dates
library(plotly) # to do interactive viz
library(hrbrthemes) # to access pretty ggplot extension themes
library(viridis) # for pretty colour schemes
library(htmlwidgets) # to save and embed interactive plots

#¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤

# SCRAPING PART 1: SONGS ABOUT WARS

url <- "http://en.wikipedia.org/wiki/List_of_anti-war_songs" # my link
rawpage_ni <- GET(url) # getting raw html for my url
cleanpage_ni3 <- htmlParse(rawpage_ni) # parsing the html

#¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤

# extracting all tables in the html that are relevant to me and all data within them 
star_col <- 2
end_col <- 22
tbls_ni <- map2(url, star_col:end_col, htmltab) 

# turning them into tibbles for manipulation 
tibbles <- list()
for (i in 1:length(tbls_ni)) {
tibbles[[i]] <- as_tibble(tbls_ni[[i]])  
}


# extracting tables´ titles
titles <- xpathSApply(cleanpage_ni3, "//span[@class='mw-headline']", xmlValue)
titles <- titles[1:21] # only keeping relevant ones

# and adding them to each tibbles
tibbles_complete <- Map(function(x, y) {
  x$War <- y
  x
},
tibbles, 
titles)

# I only retain those wars that are kind of explicit (i.e. American Indian Wars span over 300 years, and
# hard to assess deaths, territories, parties involved etc)
# and also remove those wars which only have one song made about them (not very interesting to analyse)
tibbles_complete_cleared <- tibbles_complete[c(1, 3, 4, 6,9:11, 13, 15, 17, 19:20)]

#¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤

# data wrangling and cleaning: I have a few tibbles and for the sake of analysis I need to merge them all in one
warsongs_df <- bind_rows(tibbles_complete_cleared) %>% 
  filter(!str_detect(Year, "\\?")) # I am removing an observation which had missing data

warsongs_df$War <- gsub(pattern = "\\n", replacement = "", x = warsongs_df$War)
warsongs_df$Year <- gsub(pattern = "\\â.*", replacement = "", x = warsongs_df$Year)
warsongs_df$Year <- gsub(pattern = "\\s.*", replacement = "", x = warsongs_df$Year)
warsongs_df$Year <- gsub(pattern = "\\s", replacement = "", x = warsongs_df$Year)
warsongs_df$Year <- gsub(pattern = "\\/.*", replacement = "", x = warsongs_df$Year)

#¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤

# let´s see how many songs were dedicated to each war
g <- ggplot(warsongs_df) +
  geom_bar(aes(x = fct_infreq(War)), fill = "tomato") +
  coord_flip() +
  labs(y = "number of songs", 
       title = "War and Music: what have we dedicated our music to?", 
       subtitle = "Data from Wikipedia") +
  labs(x = "war")

#¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤

# SCRAPING PART 2: INFO ABOUT WARS

url_2 <- "http://en.wikipedia.org/wiki/List_of_wars_by_death_toll" # my link
rawpage_deadliness <- GET(url_2) # getting raw html for my url
cleanpage_deadliness <- htmlParse(rawpage_deadliness) # parsing the html

# the largest wars are contained in a table called "modern wars" with large death tolls: this is how I extract
# the rows I need (as they match my music df)
extract_info_per_row <- function(page, node_nr, column_numbers){
  node <-  xpathSApply(cleanpage_deadliness, paste0 ("//table[7]//tbody[1]//tr[", node_nr, "]"))[[1]]
  
  cells <- c()
  for(i in 1:length(column_numbers)){
    cells[i] <- xpathSApply(node, paste0(".//td[",column_numbers[i],"]"), xmlValue)
  }
  return(cells)
}


rows_of_interest <- lapply(c(39,53,61,63,76,79,106,108,116,118), function(x) extract_info_per_row(page = cleanpage_deadliness, 
                                                        node_nr = x, 
                                                        column_numbers = c(1,2,4,5,6)))
wars_info <- do.call(rbind, rows_of_interest)

#turning into a tibble
wars_info <- as_tibble(wars_info)

# cleaning those names
wars_info$V5 <- gsub(pattern = "\\n", replacement = "", x = wars_info$V5)
wars_info$V1 <- gsub(pattern = "\\n", replacement = "", x = wars_info$V1)
wars_info$V2 <- gsub(pattern = "\\+.*$", replacement = "", x = wars_info$V2)
wars_info$V3 <- gsub(pattern = "\\p.*$", replacement = "2020", x = wars_info$V3)
wars_info$V2 <- gsub(pattern = "\\,", replacement = "", x = wars_info$V2)
wars_info$V1 <- gsub(pattern = "\\Viet.*$", replacement = "Vietnam War era", x = wars_info$V1) # this is to match the name in the other df

 
# Unfortunately some wars were missing from the same page, or I was unable to find data in the same format
# Thus I have manually researched the missing info on Wikipedia, on the following wars:
# c("Northern Ireland Troubles", "Falklands War", Yugoslav war (its sub-war Croatian War)", "9/11")

# As some wars have been coded differently in the two Wikipedia pages, I have merged some wars and split some others
wars_tibble_comprehensive <- wars_info %>% 
  rename(War = V1,
         Deaths = V2,
         Date = V3,
         Combatants = V4,
         Location = V5) %>% 
  separate(Date, c("beginning", "ending"), sep = c("([\\-|\\–])")) %>% 
  separate(Deaths, c("Min_deaths", "Max_deaths"), sep = c("([\\-|\\–])")) %>% 
  add_row(War = "Falklands War", Min_deaths = "907", Max_deaths = "907", beginning = "1982", ending = "1982", Combatants = "Argentina vs United Kingdom", Location = "Falklands") %>% 
  add_row(War = "The Troubles of Northern Ireland", Min_deaths = "3529", Max_deaths = "3529", beginning = "1966", ending = "1998", Combatants = "UK vs IRA", Location = "United Kingdom") %>% 
  add_row(War = "9/11", Min_deaths = "2996", Max_deaths = "2996", beginning = "2011", ending = "2011", Combatants = "Al-Qaeda", Location = "USA") %>% 
  add_row(War = "Croatian War", Min_deaths = "14000", Max_deaths = "14000", beginning = "1991", ending = "1995", Combatants = "Croatia vs Yugoslavia", Location = "Croatia") %>% 
  add_row(War = "Cold War / nuclear annihilation", Min_deaths = "3900000", Max_deaths = "8800000", beginning = "1947", ending = "1991", Combatants = "USA vs Soviet Union", Location = "Worldwide") %>% 
  add_row(War = "Gulf War(s), Iraq, 9/11, and the War on Terror", Min_deaths = "451452", Max_deaths = "1764452", beginning = "1990", ending = "2020", Combatants = "USA vs Various Middle-Eastern countries and rebel groups", Location = "Worldwide") %>% 
  add_row(War = "Yugoslav Wars", Min_deaths = "111214", Max_deaths = "140000", beginning = "1991", ending = "1995", Combatants = "Yugoslavia vs other countries", Location = "Balcans") %>% 
mutate(beginning = as.numeric(beginning),
       ending = as.numeric(ending),
       duration = ending - beginning,
       Max_deaths = as.numeric (Max_deaths),
       Min_deaths = as.numeric(Min_deaths),
       avg_death = (Min_deaths + Max_deaths)/2) 
  


# removing duplicates, i.e. Bosnian War and Croatian War are kept only as Yugoslav War. I merely included it earlier in its own row,
# trying to operate other functions such as summarise, group_by, though unsuccessfully. Useful to have it maybe to keep storage of 
# the underlying info for the Yugoslav War and the 9/11 war-music category
wars_tibble_matching_music <- wars_tibble_comprehensive[-c(7:10, 13:14),] 
  

#¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤

# Beautiful stuff: now I have two datasets with matching War id. Time to merge my dataframes to have all info in one place

all_about_war <- warsongs_df %>% 
  full_join(wars_tibble_matching_music, by = "War") 

# I still have lots of songs that are war-related but not to any specific war. I here exclude them,
# reformat my date (excluding automatically inserted month and day info), create a cumulative index
# for songs by war, unite info about song title and artist in one column.
all_about_war_without_general <- all_about_war %>% 
  filter(!str_detect(War, "General")) %>% 
  mutate(Year = ymd(Year, truncated = 2L)) %>%
  arrange(Year) %>% 
  mutate(Year = year(Year)) %>% 
  add_count(War, name = "n_songs_by_war") %>% 
  group_by(War) %>% mutate(id = row_number()) %>% 
  unite(Song, c(Song, Artist), sep = " - ")

#¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤

# THE FUN MAY BEGIN - DATA VIZ TIME (joking, it was all fun until now too ;))!!! PLOTS:

# 1. songs overtime compared to war duration
g1 <- ggplot(all_about_war_without_general, mapping = aes(x = War, y = Year, text = Song)) + 
  geom_linerange(aes(ymin = beginning, ymax = ending)) +
  geom_jitter(aes(color = "tomato"), alpha = 0.5) +
  coord_flip() +
  theme_ipsum() +
  labs(title = "Are songs made during or after a war?", subtitle = "Data from Wikipedia") +
  theme(legend.position = "none")

p1 <- ggplotly(g1, tooltip = "text") 



# 2. death toll and number of songs (plot with linear model)
g2 <- ggplot(all_about_war_without_general, aes(x = avg_death, y = n_songs_by_war)) + 
  stat_smooth(method = 'lm', aes(col = 'blue'), se = F) + 
  scale_color_discrete('Model', labels = c('red' = 'LOESS', 'blue' = 'lm')) +
  labs(title = "Correlation of war death toll and number of songs per war", 
       subtitle = "Data from Wikipedia", x = "average death", y = "number of songs per war")

p2 <- g2 + stat_sum() + 
  scale_size(range = c(1,5)) # when things can go wrong with regression: cold war many songs not many deaths / WW2 viceversa? 


# 4. number of songs and global involvement in war?
g3 <- ggplot(all_about_war_without_general) +
  geom_bar(aes(x = fct_infreq(Location)), fill = "tomato") +
  labs(y = "total number of songs", x = "War location",
       title = "Are global wars more sung about?", 
       subtitle = "Data from Wikipedia") 

# 5. cumulative distribution of songs by war
# This plot is "optional", meaning check it out if you like.
# The code is a bit tricky to run locally (the output is downloaded in Temp Appdata, which 
# usually is a hidden file in a pc. Thus here below is the code "muted".

# Library
#library(gganimate)

# ggplot(all_about_war_without_general, aes(x = Year, y = id, group = War, color = War)) +
#  geom_line(size = 1) +
#  theme_ipsum() +
#  ggtitle("The cumulative distribution of songs about wars") +
#  transition_reveal(Year)

# Save at gif:
#anim_save("cumulativesongswar.gif")

# 6. Streamgraph: a cool tool to create interactive viz that can be embedded in html
# I will use it here to create a timeline of music-making by war
library(streamgraph)

# first, I need to format my data (Streamgraph is "picky")
ambwwg_sg <- all_about_war_without_general %>% 
  select(Year, War, id) %>% 
  mutate(id = row_number()) %>% 
  group_by(Year, War) %>% 
  count()

# a graph showing how many songs were made each year per each war (I narrowed the observation as between 1860 and 1940 not much happened)
g4 <- ambwwg_sg %>% 
  filter(Year > 1940) %>% 
  streamgraph("War", "n", "Year", offset="zero", interpolate="step", scale = "continuous") %>% 
  sg_fill_brewer("PuOr") %>% 
  sg_legend(show = TRUE, label = "War: ") 

# a graph showing cumulative numbers of songs per war overtime (I kept the entire timeline here instead)
g5 <- ambwwg_sg %>%
  group_by(War) %>% mutate(id = cumsum(n)) %>% 
  streamgraph(War, "id", "Year", offset="zero", interpolate="cardinal", scale = "continuous") %>% 
  sg_fill_brewer("PuOr") %>% 
  sg_annotate(label = "American Civil War", x = 1860, y = 40, color = "black", size = 9) %>% 
  sg_annotate(label = "World War I", x = 1914, y = 70, color = "black", size = 9)%>% 
  sg_annotate(label = "World War II", x = 1941, y = 200, color = "black", size = 9)%>% 
  sg_annotate(label = "Cold War", x = 1980, y = 460, color = "black", size = 9)%>% 
  sg_annotate(label = "Vietnam War", x = 1960, y = 330, color = "black", size = 9)%>% 
  sg_annotate(label = "Korean War", x = 1950, y = 20, color = "black", size = 9)%>% 
  sg_annotate(label = "Yugoslav War", x = 1990, y = 100, color = "white", size = 9)%>% 
  sg_annotate(label = "Gulf War, War on Terror, 9/11, Iraq War", x = 1980, y = 350, color = "black", size = 9)%>% 
  sg_annotate(label = "The Troubles of Northern Ireland", x = 1970, y = 200, color = "black", size = 9)%>% 
  sg_annotate(label = "Falklands War", x = 1983, y = 400, color = "black", size = 9)%>% 
  sg_annotate(label = "Spanish Civil War", x = 1936, y = 100, color = "black", size = 9) %>% 
  sg_legend(show = TRUE, label = "War: ") 



#¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤

# AUTOMATION EXAMPLE (can be useful for an expansion of the above-presented analysis)

# SCRAPING PART 3: themes in music

main_page <- "https://en.wikipedia.org/wiki/Category:Songs_by_theme" # my link
rawpage_main <- GET(main_page) # getting raw html for my url
cleanpage_main <- htmlParse(rawpage_main) # parsing the html

# I want to scrape from several pages (each page which is a direct link in the subcategories list in the page above).
# The pages have a common url part, and a dynamic one. The dynamic part is simply a text matching the title in the list.
# Thus I will extract the titles, format them as per the url, and run a for loop to extract what I want from each page, which
# is all the songs on each page. 

# extracting the unique text at the end of the urls
list_for_urls <- xpathSApply(cleanpage_main, "//body[@class='mediawiki ltr sitedir-ltr mw-hide-empty-elt ns-14 ns-subject mw-editable page-Category_Songs_by_theme rootpage-Category_Songs_by_theme skin-vector action-view']/div[@id='content']/div[@id='bodyContent']/div[@id='mw-content-text']/div[@class='mw-category-generated']/div[@id='mw-subcategories']/div[@class='mw-content-ltr']/div[@class='mw-category']/div/ul/li", xmlValue)

# cleaning those names
list_for_urls <- gsub(pattern = "\\►  ", replacement = "", x = list_for_urls)
list_for_urls <- gsub(pattern = "\\(.*$", replacement = "", x = list_for_urls)
list_for_urls <- gsub(pattern = "\\ ", replacement = "_", x = list_for_urls)
list_for_urls <- gsub(pattern = "\\_$", replacement = "", x = list_for_urls)

# get all urls
page_urls <-  paste0("https://en.wikipedia.org/wiki/Category:", list_for_urls)

# running a for loop to extract html
page_html <- list()

for (i in 1:133) {
  cat("\rPage num.", i)
  page_html[[i]] <- GET(page_urls[i]) # query the website and return the html to the variable ‘page’
}

# parsing using lapply
page_parsed <- lapply(page_html, function(a) htmlParse(a)) # parse the html

# extracting the info I want from the pages (the song titles)
songs_all_pages <- lapply(page_parsed, function(f) list(songs_all_pages = xpathSApply(f, "//div[@class='mw-category']//div//ul//li", xmlValue)))

# removing first two sub-lists as they are lists of lists (and from title vector too)
# I am also removing all those lists that for some reasons, while scraping, did not 
# pick up the indicated songs as expected. I thought of a function to remove them by 
# a logical criteria (they all return "NULL" when printed) by failed in creating a successful one.
# gutted not to have figured out a better way!
songs_all_pages <- songs_all_pages[-c(1,2,4,12,47,69,80,91,96,97,100,103,128)] 
list_for_urls <- list_for_urls[-c(1,2,4,12,47,69,80,91,96,97,100,103,128)]

# turning all them into dataframes
songs_df <- list()
for (i in 1:length(songs_all_pages)) {
  songs_df[[i]] <- as.data.frame(songs_all_pages[i])
}


# and adding titles to them
songs_df_titled <- Map(function(x, y) {
  x$list_for_urls <- y
  x
},
songs_df, 
list_for_urls)

# merging them all in one large df 
songs_dataframe <- bind_rows(songs_df_titled, .id = "list_for_urls")
write.csv(songs_dataframe,'songs_dataframe_for_research_expansion.csv')
write.csv(all_about_war,'all_about_war.csv')
write.csv(all_about_war_without_general, "all_about_war_without_general.csv")
save.image(file='thisEnvironment.RData')
# Final notes: this dataset is far from perfect. It requires selective cleaning: for example, the data extracting allowed for the selection of other list containers in the 
# next Wikipedia page (see row 1 in the final df). I could not think of an easy way to differentiate between a song title and a list of songs title. The xpath 
# for the attribute would have been the same, and working on the text would have not been that feasible either.
# Nonetheless, I am quite satisfied with the outcome and I think there is opportunity for creating a really comprehensive dataset out of this scraped data.