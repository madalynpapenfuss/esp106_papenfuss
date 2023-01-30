##########################

# ESP 106
# Lab 3 (Monday) - graphing

##########################

#In this lab we will start by reading merging in data on economic development and indoor and outdoor air pollution. Then we will practice making some graphs with it.



#1. First read in the two csv files: gdppercapitaandgini and airpollution

#Both datasets are from Our World in Data: ourworldindata.org
#The GDP dataset has GDP per capita and the GINI index (a measure of income inequality: https://en.wikipedia.org/wiki/Gini_coefficient)
#The air pollution dataset has death rates from indoor and outdoor air pollution - units are in deaths per 100,000 people
#Indoor air pollution is the Household Air Pollution from Solid Fules
#Outdoor air pollution is split into particulate matter and ozone

#Hint: The column names are long and cumbersome (because they contain information about units et) - you might want to rename some of the columns to make them easier to work with
airpollution = read.csv("airpollution.csv")
gdppercapiandgini = read.csv("gdppercapiandgini.csv")

colnames(airpollution) <- c("Country", "Code", "Year", "Deaths_from_Outdoor_Particulate", "Deaths_from_Indoor_Solid", "Deaths_from_Outdoor_Ozone", "Deaths_from_Air_Pollution")
colnames(gdppercapiandgini) <- c("Country", "Code", "Year", "Total_Population", "Continent", "Gini_Coefficient", "Real_GDP_Per_Capita")


#2. Chose two countries that you are interested in and make a plot showing the death rates from indoor 
# air pollution and outdoor air pollution (sum of particulate matter and ozone) over time
#Distinguish the countries using different colored lines and the types of pollution using different line types
#Make sure to add a legend and appropriate titles for the axes and plot 


#Hint: you can see all the different country names using unique(x$Entity) where x is the data frame containing the air pollution data
# unique(airpollution$Country) * to determine countries
# Sweden and United States
#Then create two new data frames that contain only the rows corresponding to each of the two countries you want to look at
airpollution$total_outdoor_airpollution <- airpollution$Deaths_from_Outdoor_Particulate+airpollution$Deaths_from_Outdoor_Ozone

USAP <- airpollution[airpollution$Country == 'United States', ]
# USAP[1,] *ensuring my col was added
SAP <- airpollution[airpollution$Country == 'Sweden', ]
# SAP[1,]
#Create a new column of total outdoor air pollution deaths by summing death rates from particulate matter and ozone airpollution


#Use these to make your plot and add the lines you need

# SWEDEN
par(mfrow=c(1,1))
plot(SAP$Year, SAP$Deaths_from_Indoor_Solid, main = 'DEATHS FROM AIR POLLUTION OVER TIME',
          ylim = c(0,40), xlab = "Year", ylab = "Deaths", col='red', type = "l", lty=3, lwd=5)
lines(SAP$Year, SAP$total_outdoor_airpollution, col="darkorange", lty=3, lwd=5)

# UNITED STATES
lines(USAP$Year, USAP$total_outdoor_airpollution, col="darkgreen", lty=3, lwd=5)
lines(USAP$Year, USAP$Deaths_from_Indoor_Solid, col="blue", lty=3, lwd=5)

legend("topright", c("Swedens Deaths from Indoor Solid Pollution", "Sweden Deaths from Outdoor Pollution",
                    "United States Deaths from Indoor Solid Pollution", "United States Deaths from Outdoor Pollution"), 
       col = c("red", "darkorange", "blue", "darkgreen"),
       pch = c(19, 19), cex = 0.65, 
       box.col = "darkblue")


#Hint: you might have to set the y scale manually to make sure your plot is wide enough to show both countries. You can do this using the "ylim" argument in plot


#3. Merge the air pollution data with the gdp data using merge()
# Merge is a function that combines data across two data frames by matching ID rows
#By default merge will identify ID rows as those where column names are the same between datasets, but it is safer to specify the columns you want to merge by yourself using "by"
#In our case, we want to merge both by country (either the "Entity" or "Code" columns) and year columns
#Note that by default, the merge function keeps only the entries that appear in both data frames - that is fine for this lab. If you need for other applications, you can change using the all.x or all.y arguments to the function - check out the documentation at ?merge
#?merge
dfa <- merge(airpollution, gdppercapiandgini, by=c("Country", "Code", "Year"))
head(dfa)


#4. Make a plot with two subplots - one showing a scatter plot between log of per-capita GDP (x axis) and indoor air pollution death rate (y axis) and one showing log of per-capita GDP (x axis) and outdoor air pollution (y axis)
# scatter plot between log of per-capita GDP (x axis) and indoor air pollution death rate (y axis)
dfa
par(mfrow=c(1,2))
x <- log(dfa$Real_GDP_Per_Capita)
plot(x, dfa$Deaths_from_Indoor_Solid, xlim = c(0,15), ylim = c(0,250), xlab = "Log of Per Capita GDP", 
     ylab = "Deaths Caused by \n Indoor Air Pollutation", main = "Deaths Caused By Indoor Air Pollution \n vs Log of GDP Per Capita", 
     pch=20, cex=.5)
par(mar=c(5,6,5,5))

dfa$x <- log(dfa$Real_GDP_Per_Capita)
plot(dfa$x, dfa$total_outdoor_airpollution, xlim = c(0,15), ylim = c(0, 150), xlab = "Log of GDP Per Capita", 
     ylab = "Deaths Caused by \n Outdoor Air Pollution", main = "Deaths Caused By Outdoor Air Pollution \n vs Log of GDP Per Capita", 
     pch=20, cex=.5)
par(mar=c(5,5,5,5))

# Scatter plot between log of per-capita GDP (x axis) and outdoor air pollution (y axis)
#Make sure to add appropriate titles to the plots and axes
#Use ylim to keep the range of the y axis the same between the two plots - this makes it easier for the reader to compare across the two graphs
#STRECTH GOAL CHALLENGE - color the points based on continent. NOT REQUIRED FOR FULL POINTS - a challenge if you want to push yourself - continent info is included in the GDP dataset, but it is only listed for the year 2015
#If you are trying this and getting stuck ASK FOR HELP - there are some tips and tricks for making it easier 

#COLOR BY CONTINENT, first label all entries (not just 2015)
z <- dfa[dfa$Continent != "",  c("Country", "Continent")]
dfa$Continent = NULL
dfa = merge(dfa, z, by="Country")
z

# assign colors to variable
s <- c("red", "orange", "darkblue", "maroon", "cyan", "darkgreen")
#placing unique continent entries into dataframe (attached to colors (s))
uc <- unique(dfa$Continent)
uc <- uc[uc!= ""]
ucs <- data.frame(Continent=uc, col=s)
ucs
dfa2 <- merge(dfa, ucs, by = "Continent")
head(dfa2)

x <- log(dfa2$Real_GDP_Per_Capita)
par(mfrow=c(1,2))
plot(dfa2$x, dfa2$total_outdoor_airpollution, xlim = c(0,15), ylim = c(0, 150), xlab = "Log of GDP Per Capita", 
     ylab = "Deaths Caused by \n Outdoor Air Pollution", main = "Deaths Caused By Outdoor Air Pollution \n vs Log of GDP Per Capita", 
     col = dfa2$col, pch=20, cex=0.5)

x <- log(dfa2$Real_GDP_Per_Capita)
plot(x, dfa2$Deaths_from_Indoor_Solid, xlim = c(0,15), ylim = c(0,250), xlab = "Log of Per Capita GDP", 
     ylab = "Deaths Caused by \n Indoor Air Pollutation", main = "Deaths Caused By Indoor Air Pollution \n vs Log of GDP Per Capita",
     col = dfa2$col,  pch=20, cex=0.5)

### IN REGARDS TO WEEK 3 LAB 2, I AM CURRENTLY ON STEP FOUR AND HAVE QUESTIONS
### ABOUT HOW TO MOVE FORWARD