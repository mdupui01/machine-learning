library(ggplot2)

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

