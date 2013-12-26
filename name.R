vowels <- c('a','e','i','o','u','y')
consonants <- c('b','c','d','f','g','h','j','k','l','m','n','p','q','r','s','t','v','w','x','z')

learn <- function() {
	res <- list()
	# create unigram structure
	letterlist <- sample(0,26,rep=TRUE)
	names(letterlist) <- letters
	res$unigram <- list()
	res$unigram[['_']] <- letterlist
	for(l in letters) {
		res$unigram[[l]] <- letterlist
	}
	# get name list
	namelist <- read.table('names/yob2012.txt', sep=',', header=FALSE)
	names(namelist) <- c('name', 'gender', 'count')
	namelist$name <- tolower(as.character(namelist$name))
	# get statistics
	for(i in seq(namelist$name)) {
		split = strsplit(namelist[i,1], '')[[1]]
		# increment frequencyf of first letter
		res$unigram[['_']][[split[1]]] <- res$unigram[['_']][[split[1]]] + 1
		for(j in 2:length(split)) {
			# increment frequency of letter based on previous unigram
			res$unigram[[split[j - 1]]][[split[j]]] <- res$unigram[[split[j-1]]][[split[j]]] + 1
		}
	}
	res
}

name <- function(stats, len = FALSE, count = 1) {
	names <- c()
	for(i in seq(count)) {
		names <- append(names, onename(stats, len))
	}
	names
}
onename <- function(stats, len = FALSE) {
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