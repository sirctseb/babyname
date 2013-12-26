vowels <- c('a','e','i','o','u','y')
consonants <- c('b','c','d','f','g','h','j','k','l','m','n','p','q','r','s','t','v','w','x','z')

learn <- function() {
	res <- list()
	# get name list
	namelist <- read.table('names/yob2012.txt', sep=',', header=FALSE)
	names(namelist) <- c('name', 'gender', 'count')
	namelist$name <- as.character(namelist$name)
	numnames <- nrow(namelist)
	# get statistics
	vowelfirst <- nrow(subset(namelist, tolower(substr(name,1,1)) %in% vowels))
	nonTrailingC <- 0;
	nonTrailingV <- 0;
	vAfterC <- 0;
	cAfterV <- 0;
	for(i in seq(numnames)) {
		split = strsplit(namelist[i,1], '')[[1]]
		for(j in seq(split)) {
			if(j < length(split)) {
				if(split[[j]] %in% vowels) {
					nonTrailingV <- nonTrailingV + 1
					if(split[[j + 1]] %in% consonants) {
						cAfterV <- cAfterV + 1
					}
				} else {
					nonTrailingC <- nonTrailingC + 1
					if(split[[j + 1]] %in% vowels) {
						vAfterC <- vAfterC + 1
					}
				}
			}
		}
	}
	vAfterC <- vAfterC / nonTrailingC
	cAfterV <- cAfterV / nonTrailingV
	res$vAfterC <- vAfterC
	res$cAfterV <- cAfterV
	res$vowelFirst <- vowelfirst
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
	# generate first letter based on vowel/non-vowel first letter frequence
	if(runif(1) < stats$vowelFirst) {
		name <- sample(vowels, 1)
	} else {
		name <- sample(consonants, 1)
	}
	len <- len - 1
	# append letters until len
	while(len > 0) {
		if(tail(name, 1) %in% vowels) {
			if(runif(1) < stats$cAfterV) {
				nextType <- 'c'
			} else {
				nextType <- 'v'
			}
		} else {
			if(runif(1) < stats$vAfterC) {
				nextType <- 'v'
			} else {
				nextType <- 'c'
			}
		}
		if(nextType == 'c') {
			name <- append(name, sample(consonants,1))
		} else {
			name <- append(name, sample(vowels, 1))
		}
		len <- len - 1
	}
	paste(name, collapse = '')
}