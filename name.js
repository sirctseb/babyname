var letters = 'abcdefghijklmnopqrstuvwxyz';
var makeNames = function(stats, len, count, gender) {
	if(count === undefined) count = 1;
	var un = null;
	if(gender === undefined) {
		un = stats.ngram;
	} else if(gender[0] == 'm') {
		un = stats.ngram_m;
	} else {
		un = stats.ngram_f;
	}
	var names = [];
	for(var i = 0; i < count; i++) {
		names.push(onename(un, len))
	}
	return names;
};
// Returns a random integer between min and max
var getRandomInt = function(min, max) {
  return Math.floor(Math.random() * (max - min + 1) + min);
};
// sample from data with probabilities specified by prob
// prob neet not sum to 1 but must be non-negative values
var sample = function(data, prob) {
	if(prob !== undefined && data.length != prob.length) {
		throw "Data (" + data.length + ") and probability weights (" + prob.length + ") must be same length";
	}
	if(prob === undefined) {
		return data[getRandomInt(0, data.length)];
	}
	var cum = [prob[0]];
	for(var i = 1; i < data.length; i++) {
		cum.push(cum[i-1] + prob[i]);
	}
	var r = Math.random() * cum[cum.length - 1];
	var index = 0;
	for(index = 0; cum[index] < r; index++) {
	}
	return data[index];
};
var onename = function(ngram, len) {
	if(len === undefined) {
		// todo magic numbers to define range of name lengths
		len = getRandomInt(4,8);
	}
	// generate first letter based on frequency
	var name = sample(letters, ngram.gram);
	len--;
	// generate second letter based on previous unigram
	name = name.concat(sample(letters, ngram[name[name.length - 1]].gram));
	len--;
	// generate remaining letters based on previous bigrams
	while(len > 0) {
		name = name.concat(sample(letters, ngram[name[name.length - 2]][name[name.length - 1]].gram))
		len--;
	}
	return name;
};