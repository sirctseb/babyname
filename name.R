vowels <- c('a','e','i','o','u','y')
consonants <- c('b','c','d','f','g','h','j','k','l','m','n','p','q','r','s','t','v','w','x','z')

learn <- function() {
	res <- list()
	# get name list
	namelist <- read.table('names/yob2012.txt', sep=',', header=FALSE)
	names(namelist) <- c('name', 'gender', 'count')
	namelist$name <- tolower(as.character(namelist$name))
	namelist.f <- subset(namelist, gender == 'F')
	namelist.m <- subset(namelist, gender == 'M')
	res$unigram.f <- aggregate(namelist.f)
	res$unigram.m <- aggregate(namelist.m)
	res$unigram <- list()
	for(l in names(res$unigram.f)) {
		res$unigram[[l]] <- res$unigram.m[[l]] + res$unigram.f[[l]]
	}
	res
}
aggregate <- function(namelist) {
	# initialize unigram list
	letterlist <- sample(0,26,rep=TRUE)
	names(letterlist) <- letters
	unigram <- list()
	unigram[['_']] <- letterlist
	for(l in letters) {
		unigram[[l]] <- letterlist
	}
	# get statistics
	for(i in seq(namelist$name)) {
		split = strsplit(namelist[i,1], '')[[1]]
		# increment frequencyf of first letter
		unigram[['_']][[split[1]]] <- unigram[['_']][[split[1]]] + 1
		for(j in 2:length(split)) {
			# increment frequency of letter based on previous unigram
			unigram[[split[j - 1]]][[split[j]]] <- unigram[[split[j-1]]][[split[j]]] + 1
		}
	}
	unigram
}

name <- function(stats, len = FALSE, count = 1, gender = 'both') {
	if(gender == 'both') {
		# TODO combine data
		un <- stats$unigram
	} else if(head(gender,1) == 'm') {
		un <- stats$unigram.m
	} else {
		un <- stats$unigram.f
	}
	names <- c()
	for(i in seq(count)) {
		names <- append(names, onename(un, len))
	}
	names
}
onename <- function(unigram, len = FALSE) {
	if(len == FALSE) {
		# todo magic numbers to define range of name lengths
		len <- sample(4:8, 1)
	}
	# generate first letter based on frequency
	name <- sample(letters, 1, prob = stats$unigram[['_']])
	len <- len - 1
	# append letters until len
	while(len > 0) {
		name <- append(name, sample(letters, 1, prob = stats$unigram[[tail(name, 1)]]))
		len <- len - 1
	}
	paste(name, collapse = '')
}