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
ELP_all <- read.csv('/Users/winterb/Data/ELP/ELP_with_POS_cleaned.csv')

## Load in CMU data:

CMU <- readLines('cmudict-0.7b.txt')

## Don't need first 1:126 rows:

CMU <- CMU[-c(1:126)]

## Process:

CMU <- strsplit(CMU, split = ' +')

## Get words:

words <- sapply(CMU, FUN = function(x) x[1])

## Get last phonemes:

final <- sapply(CMU, FUN = function(x) x[length(x)])

## Make a table out of this:

CMU <- data.frame(Word = words, Final = final)

## Get rid of that final number for some of the vowels (I think for stress? - need to check):

CMU$Final <- gsub('[0-9]', '', CMU$Final)

## Make lowercase:

CMU$Word <- tolower(CMU$Word)

## Phoneme classifications:

phones <- read.table('cmudict-0.7b.phones.txt')
names(phones) <- c('Phoneme', 'Class')

## Add final class information to table:

CMU$FinalClass <- phones[match(CMU$Final, phones$Phoneme), ]$Class

## How many words?

nrow(CMU)	# 133,784


##------------------------------------------------------------------
## Trimming the data down to what is needed:
##------------------------------------------------------------------

## Merge pronunciations into BECL file:

BECL$Pron <- CMU[match(BECL$lemma, CMU$Word), ]$FinalClass

## Get rid of those for which there's no pronunciation:

nrow(BECL)	# 11,869
sum(is.na(BECL$Pron))	# 429
BECL <- filter(BECL, !is.na(BECL$Pron))
nrow(BECL)	# 11,440

## Add syllable and morpheme information:

BECL$NSyll <- ELP_all[match(BECL$lemma, ELP_all$Word), ]$NSyll
BECL$NMorph <- ELP_all[match(BECL$lemma, ELP_all$Word), ]$NMorph

## Take unisyllables:

CMUsub <- filter(BECL, NSyll == 1, NMorph == 1)
# CMUsub <- filter(BECL, NMorph == 1)

## Take only those that are 100% count or mass:

CMUsub <- table(CMUsub$lemma, CMUsub$major_class)
count_100percent <- CMUsub[, 3] / rowSums(CMUsub)
count_100percent <- names(count_100percent)[count_100percent == 1]
mass_100percent <- CMUsub[, 4] / rowSums(CMUsub)
mass_100percent <- names(mass_100percent)[mass_100percent == 1]

## Put these words into a table:

CMUsub <- data.frame(Word = c(count_100percent, mass_100percent),
	CountMass = c(rep('count', length(count_100percent)), 
		rep('mass', length(mass_100percent))))

## Put pronunciations in there:

CMUsub$FinalClass <- CMU[match(CMUsub$Word, CMU$Word), ]$FinalClass

## Final class analysis:

(xtab <- table(CMUsub$FinalClass, CMUsub$CountMass))

## Get rid of affricate since only one in mass:

xtab <- xtab[-1, ]

chisq.test(xtab)	# p = 0.0065
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

