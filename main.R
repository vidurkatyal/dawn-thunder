library(digest)
library(jsonlite)

baseURL <- 'http://codeforces.com/api/'
keys <- read.csv('cfk.csv', header=FALSE)
key <- keys[1, 1]
secret <- keys[1, 2]
#this method return rating of a user
get_rating_info <- function(handle) {
	rnd <- '123456'
	methodName <- 'user.rating'
	time <- floor(as.numeric(as.POSIXct(Sys.time())))
	requestURL <- paste0(methodName, '?apiKey=', key, '&handle=', handle, '&time=', time)
	hashInp <- paste0(rnd, '/', requestURL, '#', secret)
	hashVal <- digest(hashInp, algo=c("sha512"), serialize=FALSE)
	URL <- paste0(baseURL, requestURL, '&apiSig=', rnd, hashVal)
	json_data <- fromJSON(readLines(URL, warn=FALSE))
	return(json_data$result)
}

handles <- read.csv('handles.csv', header=FALSE)
numrows <- nrow(handles)

xrange <- c(
	as.Date("2010-01-01"),
	as.Date(Sys.time())
	)
yrange <- c(1000, 3700)
plot(xrange, yrange, type="n", xlab="Timeline", ylab="Rating")

colors <- rainbow(numrows)
plotchar <- seq(18,18+numrows,1)

for(i in 1:numrows) {
	current_user_dataset <- fromJSON(toJSON(get_rating_info(handles[i, 1])), simplifyDataFrame=TRUE)

	current_user_dataset[, 5] <- as.Date(as.POSIXct(current_user_dataset[, 5], origin="1970-01-01"))

	lines(current_user_dataset[, 5], current_user_dataset[, 7],
	 type="b", lwd=1.5, col=colors[i], pch=plotchar[i]
	 )
}

title("Codeforces Rating Graph")
legend(xrange[1], yrange[2], handles[, 1], cex=0.8, col=colors,
  	pch=plotchar, title="Users")