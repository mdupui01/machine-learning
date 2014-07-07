library(ggplot2)
library(plyr)

ufo <- read.delim("/Users/marcdupuis/src/machine_learning/data/ML_for_Hackers-master/01-Introduction/data/ufo/ufo_awesome.tsv", sep="\t", stringsAsFactors = FALSE, header = FALSE, na.strings = "")

names(ufo) <- c("DateOccurred", "DateReported", "Location", "ShortDescription", "Duration", "LongDescription")

# Remove all dates in DateOccurred and DateReported which don't have 8 digits
good.rows <- ifelse(nchar(ufo$DateOccurred) != 8 | nchar(ufo$DateReported) != 8, FALSE, TRUE)
ufo <- ufo[good.rows,]

# Converting dates to date type
ufo$DateOccurred <- as.Date(ufo$DateOccurred, format="%Y%n%d")
ufo$DateReported <- as.Date(ufo$DateReported, format="%Y%n%d")

get.location<-function(l) {
  split.location<-tryCatch(strsplit(l,",")[[1]], error = function(e) return(c(NA,NA)))
  clean.location<-gsub("^ ", "", split.location)
  if (length(clean.location) > 2) {
    return(c(NA,NA))
  }
  else {
    return(clean.location)
  }
}

city.state <- lapply(ufo$Location, get.location)
head(city.state)

# Adding the city and state information to the ufo matrix.
location.matrix <- do.call(rbind, city.state)
ufo <- transform(ufo, USCity = location.matrix[,1], USState= tolower(location.matrix[,2]), stringsAsFactors = FALSE)

# Removing all non-US states
us.states <- c('ak', 'al', 'ar', 'az', 'ca', 'co', 'ct', 'de', 'fl', 'ga', 'hi', 'ia', 'id', 'il', 'in', 'ks', 'ky', 'la', 'ma', 'md', 'me', 'mi', 'mn', 'mo', 'ms', 'mt', 'nc', 'nd', 'ne', 'nh', 'nj', 'nm', 'nv', 'ny', 'oh', 'ok', 'or', 'pa', 'ri', 'sc', 'sd', 'tn', 'tx', 'ut', 'va', 'vt', 'wa', 'wi', 'wv', 'wy')
ufo$USState <- us.states[match(ufo$USState, us.states)]
ufo$USCity[is.na(ufo$USState)] <- NA

# Creating a subset consisting only of ufo sightings from US states
ufo.us <- subset(ufo, !is.na(USState))
head(ufo.us)

# Build a histogram to look at the time distribution of the data
quick.hist <- ggplot(ufo.us, aes(x = DateOccurred)) + geom_histogram()
quick.hist + scale_x_date(major = "50 years")
print(quick.hist)
ggsave(plot=quick.hist, filename= "/Users/marcdupuis/src/machine_learning/data/quick_hist.png", height = 6, width = 8)

# Remove dates earlier than 1990
ufo.us <- subset(ufo.us, DateOccurred >= as.Date("1990-01-01"))

# Add a new column indicating only the month of the sighting
ufo.us$YearMonth <- strftime(ufo.us$DateOccurred, format = "%Y-%m")

# Aggregate the data by state and month/year and fill in the months with no sightings with zeros
sightings.counts <- ddply(ufo.us, .(USState, YearMonth), nrow)
date.range <- seq.Date(from = as.Date(min(ufo.us$DateOccurred)), to = as.Date(max(ufo.us$DateOccurred)), by="month")
date.strings <- strftime(date.range, "%Y-%m")
states.date <- lapply(us.states, function(s) cbind(s, date.strings))
states.dates <- data.frame(do.call(rbind, states.date), stringsAsFactors = FALSE)
all.sightings <- merge(states.dates, sightings.counts, by.x=c("s", "date.strings"), by.y=c("USState", "YearMonth"), all = TRUE)

# Change the column names of all.sightings

names(all.sightings) <- c("State", "YearMonth", "Sightings")
all.sightings$Sightings[is.na(all.sightings$Sightings)] <- 0
all.sightings$YearMonth <- as.Date(rep(date.range, length(us.states)))
all.sightings$State <- as.factor(toupper(all.sightings$State))

# Visualization the data

state.plot <- ggplot(all.sightings, aes(x=YearMonth, y=Sightings)) +
  geom_line(aes(color="darkblue")) +
  facet_wrap(~State, nrow=10, ncol=5) +
  theme_bw() +
  scale_color_manual(values=c("darkblue"="darkblue"), guide="none") +
  xlab("Time") + ylab("Number of Sightings") +
ggsave(plot=state.plot, filename="/Users/marcdupuis/src/machine_learning/data/ufo_sightings.pdf", width=14, height=8.5)
