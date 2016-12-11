## Bodo Winter
## November 29, 2016
## First look at BECL Bochum English Count Lexicon data and iconicity

##------------------------------------------------------------------
## Pre-processing:
##------------------------------------------------------------------

library(dplyr)
library(stringr)
library(party)
options(stringsAsFactors = F)

## Load in data:

setwd('/Users/winterb/Research/count_mass/')
BECL <- read.csv('BECL.csv')
ELP_pron <- read.csv('/Users/winterb/Data/ELP/ELP_pronunciations.csv')
ELP_all <- read.csv('/Users/winterb/Data/ELP/ELP_with_POS_cleaned.csv')



##------------------------------------------------------------------
## Trimming the data down to what is needed:
##------------------------------------------------------------------

## Merge pronunciations into BECL file:

BECL$Pron <- ELP_pron[match(BECL$lemma, ELP_pron$Word), ]$Pron

## Get rid of those for which there's no pronunciation:

nrow(BECL)	# 11,869
sum(is.na(BECL$Pron))	# 806
BECL <- filter(BECL, !is.na(BECL$Pron))
nrow(BECL)	# 11,063

## Add syllable and morpheme information:

BECL$NSyll <- ELP_all[match(BECL$lemma, ELP_all$Word), ]$NSyll
BECL$NMorph <- ELP_all[match(BECL$lemma, ELP_all$Word), ]$NMorph

## Take unisyllables:

uni <- filter(BECL, NSyll == 1, NMorph == 1)

## Take only those that are 100% count or mass:

uni <- table(uni$lemma, uni$major_class)
count_100percent <- uni[, 3] / rowSums(uni)
count_100percent <- names(count_100percent)[count_100percent == 1]
mass_100percent <- uni[, 4] / rowSums(uni)
mass_100percent <- names(mass_100percent)[mass_100percent == 1]

## Put these words into a table:

uni <- data.frame(Word = c(count_100percent, mass_100percent),
	CountMass = c(rep('count', length(count_100percent)), 
		rep('mass', length(mass_100percent))))

## Put pronunciations in there:

uni$Pron <- ELP_pron[match(uni$Word, ELP_pron$Word), ]$Pron

## Function for getting matrix of consonants:

get_consonants <- function(string) {
	all_res <- c()
	patterns <- c('T', 'D', 'f', 'v', 'p', 't[^S]|t$',
		'k', 'b', 'd[^Z]|d$', 'g', 'S', 'Z|dZ', 's', 'z', 'r', 'l',
		'm', 'n', 'N', 'w', 'j', 'h', 'tS')
		# Z|dZ are together since they are not differentiated for the non-words either
	
	for (i in 1:length(patterns)) {
		all_res <- cbind(all_res,
			str_count(string, patterns[i]))
		}

	colnames(all_res) <- c('T', 'D', 'f', 'v', 'p', 't',
		'k', 'b', 'd', 'g', 'S', 'Z', 's', 'z', 'r', 'l',
		'm', 'n', 'N', 'w', 'j', 'h', 'tS')

	return(all_res)
	}

## Create matrix:

uni <- cbind(uni, get_consonants(uni$Pron))

## Final phoneme:

uni$Final <- substr(uni$Pron, nchar(uni$Pron), nchar(uni$Pron))
uni <- uni[uni$Final != '`', ]
table(uni$Final, uni$CountMass)
uni$FinalClass <- 'vowel'
uni[uni$Final %in% c('Z', 'S', 's', 'T', 'f', 'v', 'z'), ]$FinalClass <- 'fricative'
uni[uni$Final %in% c('k', 't', 'd', 'p', 'g', 'b'), ]$FinalClass <- 'stop'
uni[uni$Final %in% c('n', 'N', 'm'), ]$FinalClass <- 'nasal'
uni[uni$Final %in% c('l', 'r'), ]$FinalClass <- 'liquid'

## Final class analysis:

(xtab <- table(uni$FinalClass, uni$CountMass))
chisq.test(xtab)	# p = 0.045
round(prop.table(xtab, 1)[, 2], 2)
(xprop <- round(prop.table(xtab, 2), 2))

## Analysis of studentized Pearson residuals reveals which are significantly different:

chisq.test(xtab)$stdres

## Plot fricative / stop means:

round(prop.table(xtab[c(1, 4), ], 2), 2)
chisq.test(xtab[c(1, 4), ])

## To make a plot, create a difference score:

xdiffs <- xprop[, 2] - xprop[, 1]
xdiffs <- sort(xdiffs, decreasing = T)
xcols <- colorRampPalette(c('goldenrod3', 'steelblue'))
xcols <- xcols(5)

## Set bar width:

xfac <- 0.1

## Make a plot of the means, raw values:

quartz('', 9, 7.5)
par(mai = c(1.75, 2, 1, 0.5))
plot(1, 1,
	type = 'n', xaxt = 'n', yaxt = 'n', xlab = '', ylab = '', bty = 'n',
	xlim = c(1, 6), ylim = c(-0.18, 0.18))
axis(side = 2, at = seq(-0.15, 0.15, 0.05), las = 2, font = 2,
	lwd = 3, lwd.ticks = 3, cex.axis = 2, labels = paste0(seq(-15, 15, 5), '%'))
mtext('Percentage Difference', side = 2, line = 6, font = 2, cex = 3)
abline(h = 0, lwd = 3)
for (i in 1:5) {
	rect(xleft = i + xfac, xright = i + 1 - xfac,
		ybottom = 0, ytop = xdiffs[i],
		col = xcols[i], lwd = 2)
	if (xdiffs[i] > 0) {text(x = i + 0.5, y = xdiffs[i] + 0.015,
		labels = names(xdiffs)[i], font = 2, cex = 1.25) }
	if (xdiffs[i] < 0) {text(x = i + 0.5, y = xdiffs[i] - 0.015,
		labels = names(xdiffs)[i], font = 2, cex = 1.25) }
	}

