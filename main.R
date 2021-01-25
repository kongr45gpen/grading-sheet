library(plyr)
library(vioplot)

csv_files <- list.files(pattern = ".csv$")

#grades_specific <- matrix(ncol = length(csv_files), nrow = 15)R

names <- data.frame("id", "name")

MAX_SCALING <- 10
MIN_SCALING <- 7

for (f in seq_along(csv_files)) {
  datum <- read.csv(csv_files[f], header = TRUE, skip = 1)

  if (f == 1) {
    names <- datum[,c(1,7)]
    names <- names[!apply(is.na(names) | names == "", 1, all),]
    names(names) <- c("id", "name")
    names <- names[!duplicated(names["name"]),]
    rownames(names) <- names[,"id"]
    
    total_grades <- names[,FALSE]
    total_grades <- as.data.frame(t(as.matrix(total_grades))) # Transpose
  }
  
  grades <- data.frame(id=datum[,1], value=datum[,15])
  grades <- grades[!apply(is.na(grades[,"id"]) | grades == "", 1, all),]
  rownames(grades) = grades[,"id"]
  grades <- grades[!is.na(match(grades[,"id"],names$id)),]
  grades[,"value"] <- suppressWarnings(as.numeric(as.character(grades[,"value"])))
  grades <- grades[!is.na(grades[,"value"]),]
  grades <- grades[grades[,"value"] > 0,]
  
  min <- min(grades$value)
  max <- max(grades$value)
  
  # Grade scaling
  # grades$value <- (grades$value - min) / (max - min) * (MAX_SCALING - MIN_SCALING) + MIN_SCALING
  
  total_grades[f,] <- NA
  total_grades[f, rownames(grades)] <- grades[,"value"]
}


colnames(total_grades) <- names[,"name"]

# Reverse order (top to bottom)
total_grades <- total_grades[,seq(dim(total_grades)[2],1),]


par(mar = c(4, 35, 4, 1) + 0.1);
boxplot(total_grades, horizontal=TRUE, las=1,col=c('powderblue', 'gold'), mar = c(5, 4, 4, 2), outline=FALSE)
#grid(nx=5, ny=20)

means <- colMeans(total_grades,na.rm = TRUE)
means <- means[order(means)]