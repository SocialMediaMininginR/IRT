# Finalized IRT code for book.

# First, my stuff

load(file="C:/Projects/Text Analysis/choice")
load(file="C:/Projects/Text Analysis/life")
load(file="C:/Projects/Text Analysis/life2")

life <- c(life, life2)

# About 2,000 #prolife and #prochoice tweets
length(choice)
length(life)
choice <- choice[1:length(life)]

# numeric marker for hashtags
hash <- rep(1, length(choice))
hash <- c(hash, rep(0, length(life)))

# generate a data frame of the list of tweets
abortion_tweets <- c(choice, life)


###########################################################
# now, book stuff

# generate a data frame from the list of tweets
require(twitteR)
twtsdf<- twListToDF(abortion_tweets)

twtsdf$hash<- hash

# drop retweets
twtsdf<- twtsdf[twtsdf$isRetweet == FALSE,]

# drop un-necessary variables
# Drop un-needed variables from the data frame
 keeps <- c("text", "id", "retweetCount", "isRetweet", "screenName", "hash")
twtsdf <- twtsdf[,keeps]

list.vector.words<- list()
allwords<- NULL

names<- NULL
	for (i in 1:dim(twtsdf)[1]){
	  each.vector<- strsplit(twtsdf$text[i], split="")
	  names<- c(names, twtsdf$screenName[i])
	  allwords<- c(allwords, each.vector)
	  list.vector.words[[i]] <- each.vector
}


outcome<- twtsdf$hash

require(tm)
 dat.tm <- Corpus(VectorSource(list.vector.words))	# make a corpus
 dat.tm <- tm_map(dat.tm, tolower)		# convert all words to lowercase
 dat.tm <- tm_map(dat.tm, removePunctuation)			# remove punctuation
 dat.tm <- tm_map(dat.tm, removeWords, words=c("prochoice"))	# remove the hashtags
 dat.tm <- tm_map(dat.tm, removeWords, words=c("prolife")) 	# remove the hashtags
 dat.tm <- tm_map(dat.tm, stripWhitespace)	# remove extra white space
 dat.tm <- tm_map(dat.tm, stemDocument)		# stem all words

# create a bigram tokenizer using the RWeka package
 require(RWeka)
BigramTokenizer<- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
# create the document-term matrix
datmat<- DocumentTermMatrix(dat.tm, control = list(tokenize = BigramTokenizer))
dat<- as.matrix(datmat)
# Add user names as rownames to matrix
rownames(dat) <- names

# first, set all values in the matrix that are greater than 1 to 1
dat[dat>1] <- 1

# Examine col sums to remove sparse/scarce bigrams
word.usage<- colSums(dat)
table(word.usage)
threshold <- 22	# set a threshold
tokeep <- which(word.usage>threshold)	# find which column sums are above the threshold
# keep all rows but the first (the column sums) and only columns with sums greater than the threshold
dat.out<- dat[,tokeep]

# pull out list of words
words<- colnames(dat.out)
# aggregate by rowname (i.e. twitter user name), and sum rows with the same user name
dat.agg<- aggregate(dat.out, list(rownames(dat.out)), sum)
# aggregating makes a variable called Group.1; turn this back into the matrix rowname
names<- dat.agg$Group.1
dat.agg<- as.matrix(dat.agg[,2:dim(dat.agg)[2]])
# set cells greater than 1 back to 1
dat.agg[dat.agg>1] <- 1
rownames(dat.agg) <- names

# aggregate hash score vector
outcomes<- as.matrix(twtsdf$hash)
rownames(outcomes) <- rownames(dat.out)
outcomes.agg <- aggregate(outcomes, list(rownames(outcomes)), mean)
hashscores<- round(outcomes.agg$V1)

# drop users with few bigrams
num.zero <- rowSums(dat.agg==0)

# explore data by making a table; can inform choice of cutoff
table(num.zero)
# the number of columns of the document bigram matrix
num_cols <- dim(dat.out)[2]
# users must have used this many bigrams to scale
cutoff <- 15
# create a list of authors to keep
authors_tokeep <- which(num.zero <(num_cols-cutoff))
# keep only users with 2 bigrams
dat.drop <- dat.out[authors_tokeep,]
# similarly, drop those users from the vector of hashtags
outcome <- outcome[authors_tokeep]

require(pscl)
rc <- rollcall(data=dat.drop)   	# sets up the data
# executes the model (this may take several minutes)
jmod <- ideal(rc,store.item=T)

scaled.positions<- data.frame(jmod$xbar)   	# make a new data frame of the scale
scaled.positions$users <- rownames(dat.drop)  #add user names
colnames(scaled.positions) <- "scale"  		# give the single variable a good name
head(scaled.positions)

hist(jmod$xbar, main= "Scaled Positions of Twitter Users", xlab= "relative position")

scaled.positions[scaled.positions$scale> 0.9,]

plot(jmod$betabar[,2], jmod$betabar[,1],
	  xlab="difficulty", ylab="discrimination", main="Bigrams")


# identify which words have large discrimination parameters
# abs() returns the absolute value
 t <- which(abs(jmod$betabar[,1]) >1)
twords<- colnames(dat.drop)[t]
tnums<- jmod$betabar[,1][t]
# make a data frame of the discriminating words
require(plyr)
bigwords<- data.frame(twords, tnums)
bigwords<- arrange(bigwords,desc(bigwords$tnums))

