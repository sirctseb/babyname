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
	res$ngram.f <- aggregate(namelist.f)
	res$ngram.m <- aggregate(namelist.m)
	res$ngram <- list()
	res$ngram$gram <- res$ngram.m$gram + res$ngram.f$gram
	for(l in letters) {
		res$ngram[[l]]$gram <- res$ngram.m[[l]]$gram + res$ngram.f[[l]]$gram
		for(d in letters) {
			res$ngram[[l]][[d]]$gram <- res$ngram.m[[l]][[d]]$gram + res$ngram.f[[l]][[d]]$gram
		}
	}
	res
}
aggregate <- function(namelist) {
	# initialize unigram list
	letterlist <- sample(0,26,rep=TRUE)
	names(letterlist) <- letters
	ngram <- list()
	ngram$gram <- letterlist
	for(l in letters) {
		ngram[[l]] <- list()
		ngram[[l]]$gram <- letterlist
		for(d in letters) {
			ngram[[l]][[d]]$gram <- letterlist
		}
	}
	# get statistics
	for(i in seq(namelist$name)) {
		split = strsplit(namelist[i,1], '')[[1]]
		# increment frequency of first letter
		ngram$gram[[split[1]]] <- ngram$gram[[split[1]]] + 1
		# increment frequency of letter based on previous unigram
		ngram[[split[1]]]$gram[[split[2]]] <- ngram[[split[1]]]$gram[[split[2]]] + 1
		for(j in 2:length(split)) {
			if(j > 2) {
				ngram[[split[j - 2]]][[split[j - 1]]]$gram[[split[j]]] <- ngram[[split[j - 2]]][[split[j - 1]]]$gram[[split[j]]] + 1
			}
		}
	}
	ngram
}

name <- function(stats, len = FALSE, count = 1, gender = 'both') {
	if(gender == 'both') {
		# TODO combine data
		un <- stats$ngram
	} else if(head(gender,1) == 'm') {
		un <- stats$ngram.m
	} else {
		un <- stats$ngram.f
	}
	names <- c()
	for(i in seq(count)) {
		names <- append(names, onename(un, len))
	}
	names
}
onename <- function(ngram, len = FALSE) {
	if(len == FALSE) {
		# todo magic numbers to define range of name lengths
		len <- sample(4:8, 1)
	}
	# generate first letter based on frequency
	name <- sample(letters, 1, prob = ngram$gram)
	len <- len - 1
	# generate second letter based on previous unigram
	# TODO train these only on first letter unigrams?
	name <- append(name, sample(letters, 1, prob = ngram[[tail(name, 1)]]$gram))
	len <- len - 1
	# generate remaining letters based on previous bigrams
	while(len > 0) {
		name <- append(name, sample(letters, 1, prob = ngram[[head(tail(name, 2), 1)]][[tail(name, 1)]]$gram))
		len <- len - 1
	}
	paste(name, collapse = '')
}