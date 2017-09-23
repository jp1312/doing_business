
setwd("C:/Users/pc/Documents/slowdata/R/doing_business")


# Reading about the internazionalization of italian SME (Small and Medium Enterprises, < 250 employees) I discovered an interesting indicator provided by the World Bank.
# It is called 'Ease of doing business' and it is used to rank economies worldwide on their business-friendliness regulations (1=most business-friendly regulations)
# Rankings are determined by sorting the aggregate 'distance to frontier' scores on 10 topics, each consisting of several indicators, giving equal weigth to each topic.
# More information on the methodologies can be found here http://www.doingbusiness.org/methodology

# In R there is a package to access World Bank data so you can avoid the hassle of downloading, importing, reformatting missing values and so on...

# Access data
library(WDI)
WDIsearch('doing business') # when you don't know the exact name of the indicator you can use this look-up function with keywords
dat <- WDI(country = "all", 
           indicator = "IC.BUS.EASE.XQ", 
           start = 2016, 
           end = 2016)

dat[150,'country'] <- 'North Korea' # fixing korea name


# Fine, but I'd like to have all the 10 components behind the index
WDIsearch('Dealing with') # when you don't know the exact name of the indicator you can use this look-up function with keywords
dim1 <- WDI(country = "all", 
           indicator = "IC.FRM.XQ", 
           start = 2016, 
           end = 2016) # apparently these dimesions are not available from API


# I downloaded them manually from http://www.doingbusiness.org/data
library(readxl)
ease_dim <- read_excel(path = "./data/Reports.xlsx", sheet = 1, col_names = TRUE, na = "", skip = 0)
keep <- grep(pattern = "Rank", x = names(ease_dim)) # keep only ranking variables
ease_dim <- ease_dim[,c(1,2,keep)]
head(ease_dim)
class(ease_dim)

# Unfortunately there are no iso2c codes in here
dat$iso2c
length(dat$country)
length(ease_dim$Economy) # probably regions do not have ranking by single dimension

sum(ease_dim$Economy %in% dat$country) # 176 country names out of 212 are matched
ease_dim[!ease_dim$Economy %in% dat$country,'Economy'] # the ones not matched are Cities, or countries with different names, let's fix this..
which(!ease_dim$Economy %in% dat$country)
ease_dim[which(!ease_dim$Economy %in% dat$country)[c(7, 8,9,10,15,18,21,29,30,36)], 'Economy'] <- c('Congo, Dem. Rep.', 'Congo, Rep.', "Cote d'Ivoire", 'Egypt, Arab Rep.', 'Iran, Islamic Rep.', 'Korea, Rep.', 'Micronesia, Fed. Sts.', 'Sao Tome and Principe', 'St. Kitts and Nevis', 'Yemen, Rep.')

# convert ranking columns to numeric
for(i in 3:ncol(ease_dim)) {
  ease_dim[,i] <- as.numeric(unlist(ease_dim[,i]))
}

# now that names  match we can merge
dat2 <- merge(x = dat, y = ease_dim, by.x = 'country', by.y = 'Economy')

# Are we getting the same data for the overall doingBusiness index??
dat2[,c('IC.BUS.EASE.XQ', 'Rank')] # well, actually not. Probably info from API are not updated, let's keep what I found on the website (as of 29/03/2017)



## world admnitrative borders

# access borders from naturalearth website
url <- "http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_admin_0_countries.zip"
tmp <- tempdir()
file <- basename(url)
download.file(url, file)
unzip(file, exdir = tmp)

library(rgdal)
library(sp)

countries <- readOGR(dsn = tmp, 
                     layer = "ne_50m_admin_0_countries", 
                     encoding = "UTF-8",
                     verbose = FALSE)

# spatial join to enrich country data with WB ease of business indicators
sum(!dat2$iso2c %in% countries$iso_a2)
dat2[which(!dat2$iso2c %in% countries$iso_a2), c('iso2c', 'country')] 
countries@data[countries@data$name=='Kosovo','iso_a2']
countries@data$iso_new <- as.character(countries@data$iso_a2)
countries@data[countries@data$name=='Kosovo','iso_new'] <- 'XK' # Fixing Kosovo iso code...

# Fixing Somalia       
countries@data[countries@data$name=='Somaliland','iso_a2']
dat2[which(dat2$country=='Somalia'),'iso2c']
countries@data[countries@data$name=='Somaliland','iso_new'] <- 'SO' # Fixing Somalia
countries@data$iso_new2 <- as.factor(countries@data$iso_new)


# let's merge
plot.data <- merge(countries, 
                    dat2, 
                    by.x = "iso_new2", 
                    by.y = "iso2c",
                    sort = TRUE)


## Prepare layer for overall ranking


# Prepare layers
library(leaflet)
library(RColorBrewer)
q1 <- quantile(x = plot.data@data$Rank, probs = seq(0, 1, 0.1), na.rm = TRUE)
popup1 <- paste0("<strong>Country: </strong>", 
                        plot.data$name, 
                        "<br><strong>", 
                        'Ease of Doing Business Rank', 
                        ", ", 
                        as.character(2016), 
                        ": </strong>", 
                        plot.data[['Rank']])

## Prepare layer for 'Start a Business' ranking
# find breaks for choropleth map colors
q2 <- quantile(x = plot.data@data[,'Starting a business-Rank'], probs = seq(0, 1, 0.1), na.rm = TRUE)
pal2 <- colorBin(rev(brewer.pal(n = 9, name = "Greens")), bins = q2)
# popus
popup2 <- paste0("<strong>Country: </strong>", 
                 plot.data$name, 
                 "<br><strong>", 
                 'Starting a Business Rank', 
                 ", ", 
                 as.character(2016), 
                 ": </strong>", 
                 plot.data[['Starting a business-Rank']])



## Prepare layer for 'Dealing with Construction Permits-Rank' ranking
# find breaks for choropleth map colors
q3 <- quantile(x = plot.data@data[,'Dealing with Construction Permits-Rank'], probs = seq(0, 1, 0.1), na.rm = TRUE)
pal3 <- colorBin(rev(brewer.pal(n = 9, name = "Greens")), bins = q3)
# popus
popup3 <- paste0("<strong>Country: </strong>", 
                 plot.data$name, 
                 "<br><strong>", 
                 'Dealing with Construction Permits-Rank', 
                 ", ", 
                 as.character(2016), 
                 ": </strong>", 
                 plot.data[['Dealing with Construction Permits-Rank']])


## Prepare layer for 'Dealing with Construction Permits-Rank' ranking
# find breaks for choropleth map colors
q4 <- quantile(x = plot.data@data[,'Getting Electricity-Rank'], probs = seq(0, 1, 0.1), na.rm = TRUE)
pal4 <- colorBin(rev(brewer.pal(n = 9, name = "Greens")), bins = q4)
# popus
popup4 <- paste0("<strong>Country: </strong>", 
                 plot.data$name, 
                 "<br><strong>", 
                 'Getting Electricity-Rank', 
                 ", ", 
                 as.character(2016), 
                 ": </strong>", 
                 plot.data[['Getting Electricity-Rank']])

## Prepare layer for 'Registering Property-Rank' ranking
# find breaks for choropleth map colors
q5 <- quantile(x = plot.data@data[,'Registering Property-Rank'], probs = seq(0, 1, 0.1), na.rm = TRUE)
pal5 <- colorBin(rev(brewer.pal(n = 9, name = "Greens")), bins = q5)
# popus
popup5 <- paste0("<strong>Country: </strong>", 
                 plot.data$name, 
                 "<br><strong>", 
                 'Getting Electricity-Rank', 
                 ", ", 
                 as.character(2016), 
                 ": </strong>", 
                 plot.data[['Registering Property-Rank']])



## Prepare layer for Getting Credit-Rank
# find breaks for choropleth map colors
q6 <- quantile(x = plot.data@data[,'Getting Credit-Rank'], probs = seq(0, 1, 0.1), na.rm = TRUE)
pal6 <- colorBin(rev(brewer.pal(n = 9, name = "Greens")), bins = q6)
# popus
popup6 <- paste0("<strong>Country: </strong>", 
                 plot.data$name, 
                 "<br><strong>", 
                 'Getting Credit-Rank', 
                 ", ", 
                 as.character(2016), 
                 ": </strong>", 
                 plot.data[['Getting Credit-Rank']])


## Prepare layer for 'Dealing with Construction Permits-Rank' ranking
# find breaks for choropleth map colors
q7 <- quantile(x = plot.data@data[,'Protecting Minority Investors-Rank'], probs = seq(0, 1, 0.1), na.rm = TRUE)
pal7 <- colorBin(rev(brewer.pal(n = 9, name = "Greens")), bins = q7)
# popus
popup7 <- paste0("<strong>Country: </strong>", 
                 plot.data$name, 
                 "<br><strong>", 
                 'Protecting Minority Investors-Rank', 
                 ", ", 
                 as.character(2016), 
                 ": </strong>", 
                 plot.data[['Protecting Minority Investors-Rank']])


## Prepare layer for 'Dealing with Construction Permits-Rank' ranking
# find breaks for choropleth map colors
q8 <- quantile(x = plot.data@data[,'Paying Taxes-Rank'], probs = seq(0, 1, 0.1), na.rm = TRUE)
pal8 <- colorBin(rev(brewer.pal(n = 9, name = "Greens")), bins = q8)
# popus
popup8 <- paste0("<strong>Country: </strong>", 
                 plot.data$name, 
                 "<br><strong>", 
                 'Paying Taxes-Rank', 
                 ", ", 
                 as.character(2016), 
                 ": </strong>", 
                 plot.data[['Paying Taxes-Rank']])

## Prepare layer for 'Dealing with Construction Permits-Rank' ranking
# find breaks for choropleth map colors
q9 <- quantile(x = plot.data@data[,'Trading Across Borders-Rank'], probs = seq(0, 1, 0.1), na.rm = TRUE)
pal9 <- colorBin(rev(brewer.pal(n = 9, name = "Greens")), bins = q9)
# popus
popup9 <- paste0("<strong>Country: </strong>", 
                 plot.data$name, 
                 "<br><strong>", 
                 'Trading Across Borders-Rank', 
                 ", ", 
                 as.character(2016), 
                 ": </strong>", 
                 plot.data[['Trading Across Borders-Rank']])


## Prepare layer for 'Dealing with Construction Permits-Rank' ranking
# find breaks for choropleth map colors
q10 <- quantile(x = plot.data@data[,'Enforcing Contracts-Rank'], probs = seq(0, 1, 0.1), na.rm = TRUE)
pal10 <- colorBin(rev(brewer.pal(n = 9, name = "Greens")), bins = q10)
# popus
popup10 <- paste0("<strong>Country: </strong>", 
                 plot.data$name, 
                 "<br><strong>", 
                 'Enforcing Contracts-Rank', 
                 ", ", 
                 as.character(2016), 
                 ": </strong>", 
                 plot.data[['Enforcing Contracts-Rank']])

## Prepare layer for 'Dealing with Construction Permits-Rank' ranking
# find breaks for choropleth map colors
q11 <- quantile(x = plot.data@data[,'Resolving Insolvency-Rank'], probs = seq(0, 1, 0.1), na.rm = TRUE)
q11 # breaks are not unique
manual_q11 <- c(1,21.2,40.2, 58.8, 77.4, 96.0, 115.6, 134.2, 152.8, 160, 169.0)
pal11 <- colorBin(rev(brewer.pal(n = 9, name = "Greens")), bins = manual_q11)
# popus
popup11 <- paste0("<strong>Country: </strong>", 
                  plot.data$name, 
                  "<br><strong>", 
                  'Resolving Insolvency-Rank', 
                  ", ", 
                  as.character(2016), 
                  ": </strong>", 
                  plot.data[['Resolving Insolvency-Rank']])


## Prepare map background
stamen_tiles <- "http://{s}.tile.stamen.com/toner-lite/{z}/{x}/{y}.png"
stamen_attribution <- 'Map tiles by <a href="http://stamen.com">Stamen Design</a>, under <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a>. Data by <a href="http://openstreetmap.org">OpenStreetMap</a>, under <a href="http://www.openstreetmap.org/copyright">ODbL</a>.'

## Plot
DoingBusinessMap <-
leaflet(data = plot.data) %>%
  addTiles(urlTemplate = stamen_tiles,  
           attribution = stamen_attribution) %>%
  setView(0, 0, zoom = 3) %>%
  addPolygons(fillColor = ~pal1(plot.data[['Rank']]), 
              fillOpacity = 0.8, 
              color = "#BDBDC3", 
              weight = 1, 
              popup = popup1,
              group = "<span style='color: #7f0000; font-size: 11pt'><strong>Ease of Doing Business Rank</strong></span>") %>%
  addPolygons(fillColor = ~pal2(plot.data[['Starting a business-Rank']]), 
              fillOpacity = 0.8, 
              color = "#BDBDC3", 
              weight = 1, 
              popup = popup2,
              group = "Starting a Business-Rank") %>%
  addPolygons(fillColor = ~pal3(plot.data[['Dealing with Construction Permits-Rank']]), 
              fillOpacity = 0.8, 
              color = "#BDBDC3", 
              weight = 1, 
              popup = popup3,
              group = "Dealing with Construction Permits-Rank") %>%
  addPolygons(fillColor = ~pal4(plot.data[['Getting Electricity-Rank']]), 
              fillOpacity = 0.8, 
              color = "#BDBDC3", 
              weight = 1, 
              popup = popup4,
              group = "Getting Electricity-Rank") %>%
  addPolygons(fillColor = ~pal5(plot.data[['Registering Property-Rank']]), 
              fillOpacity = 0.8, 
              color = "#BDBDC3", 
              weight = 1, 
              popup = popup5,
              group = "Registering Property-Rank") %>%
  addPolygons(fillColor = ~pal6(plot.data[['Getting Credit-Rank']]), 
              fillOpacity = 0.8, 
              color = "#BDBDC3", 
              weight = 1, 
              popup = popup6,
              group = "Getting Credit-Rank") %>%
  addPolygons(fillColor = ~pal7(plot.data[['Protecting Minority Investors-Rank']]), 
              fillOpacity = 0.8, 
              color = "#BDBDC3", 
              weight = 1, 
              popup = popup7,
              group = "Protecting Minority Investors-Rank") %>%
  addPolygons(fillColor = ~pal8(plot.data[['Paying Taxes-Rank']]), 
              fillOpacity = 0.8, 
              color = "#BDBDC3", 
              weight = 1, 
              popup = popup8,
              group = "Paying Taxes-Rank") %>%
  addPolygons(fillColor = ~pal9(plot.data[['Trading Across Borders-Rank']]), 
              fillOpacity = 0.8, 
              color = "#BDBDC3", 
              weight = 1, 
              popup = popup9,
              group = "Trading Across Borders-Rank") %>%
  addPolygons(fillColor = ~pal10(plot.data[['Enforcing Contracts-Rank']]), 
              fillOpacity = 0.8, 
              color = "#BDBDC3", 
              weight = 1, 
              popup = popup10,
              group = "Enforcing Contracts-Rank") %>%
  addPolygons(fillColor = ~pal11(plot.data[['Resolving Insolvency-Rank']]), 
              fillOpacity = 0.8, 
              color = "#BDBDC3", 
              weight = 1, 
              popup = popup11,
              group = "Resolving Insolvency-Rank") %>%
  addLayersControl(
    baseGroups = c("<span style='color: #7f0000; font-size: 11pt'><strong>Ease of Doing Business Rank</strong></span>", 
                   "Starting a Business-Rank",
                   "Dealing with Construction Permits-Rank",
                   "Getting Electricity-Rank",
                   "Registering Property-Rank",
                   "Getting Credit-Rank",
                   "Protecting Minority Investors-Rank",
                   "Paying Taxes-Rank",
                   "Trading Across Borders-Rank",
                   "Enforcing Contracts-Rank",
                   "Resolving Insolvency-Rank"
    ),
    options = layersControlOptions(collapsed = FALSE)) %>%
  addLegend(position = 'bottomleft', 
            colors = rev(brewer.pal(n = 9, name = 'Greens')), 
            labels = paste0(rep("<span style='font-size: 8pt'>", 9), seq(10,90,10), rep("<sup>th</sup></span>",9)),  
            opacity = 0.8,      ##transparency again
            title = "<span style='font-size: 9pt'> Top-Percentiles</span>")

library(htmlwidgets)
# saveWidget(DoingBusinessMap, "doingbiz.html", selfcontained = FALSE)
