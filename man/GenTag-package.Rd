\name{GenTag-package}
\alias{GenTag-package}
\alias{GenTag}
\docType{package}
\title{
\packageTitle{GenTag}
}
\description{
\packageDescription{GenTag}
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{GenTag}
\packageIndices{GenTag}

Implement a coherent and flexible protocol for animal color tagging. 'GenTag' provides a simple computational routine with low CPU usage to create color sequences for animal tag. First, a single-color tag sequence is created from an algorithm selected by the user, followed by verification of the combination uniqueness. Three methods to produce color tag sequences are provided. Users can modify the main function core to allow a wide range of applications.
}
\author{
\packageAuthor{GenTag}

Maintainer: \packageMaintainer{GenTag}
}
\references{
Alisauskas & Lindberg  (2002). Effects of neckbands on survival and fidelity of White-fronted and Canada geese captured as non-breeding adults. doi: 10.1080/02664760120108575
Broughton (2015). Low incidence of leg and foot injuries in colour-ringed Marsh tits Poecile palustris. doi: 10.1080/03078698.2015.1059610
Burley (1986). Sex-ratio manipulation in color-banded populations of Zebra finches. doi: 10.1111/j.1558-5646.1986.tb05744.x
Burley, Krantzberg & Radman (1982). Influence of colour-banding on the conspecific preferences of Zebra finches. doi: 10.1016/S0003-3472(82)80055-9
Calvo  & Furness  (1992). A review of the use and the effects of marks and devices on birds. doi: 10.1080/03078698.1992.9674036
Cresswell, Lind, Quinn, Minderman  & Whitfield, (2007). Ringing or colour-banding does not increase predation mortality in Redshanks Tringa totanus. doi: 10.1111/j.2007.0908-8857.3925.x
Cuthill, Hunt, Cleary, & Clark (1997). Colour bands, dominance, and body mass regulation in male Zebra finches (Taeniopygia guttata). doi: 10.1098/rspb.1997.0151
Fiske & Amundsen (1997). Female bluethroats prefer males with symmetric colour bands. doi: 10.1006/anbe.1996.0436
Gil, Graves, Hazon, & Wells, (1999). Male attractiveness and differential testosterone investment in Zebra finch eggs. doi: 10.1126/science.286.5437.126
Griesser, Schneider, Collis, Overs, Guppy, Guppy, Takeuchi, Collins, Peters & Hall (2012). Causes of ring-related leg injuries in birds - evidence and recommendations from four field studies. doi: 10.1371/journal.pone.0051891
Jennions (1998). The effect of leg band symmetry on female-male association in Zebra finches. doi: 10.1006/anbe.1997.0579
Johnsen, Fiske, Amundsen, Lifjeld, & Rohde (2000). Colour bands, mate choice and paternity in the Bluethroat. doi: 10.1006/anbe.1999.1274
Johnsen, Lifjeld, & Rohde (1997). Coloured leg bands affect male mate-guarding behaviour in the Bluethroat. doi: 10.1006/anbe.1996.0437
Johnson, Dalton & Burley (1993). Preferences of female American goldfinches (Carduelis tristis) for natural and artificial male traits. doi: 10.1093/beheco/4.2.138
Kosinski (2004). The removal of colour rings by Greenfinches Carduelis chloris. doi: 10.1080/03078698.2004.9674304
McCrea & Morgan (2014). Analysis of capture-recapture data. Boca Raton, Chapman and Hall/CRC.
Nietmann & Ha (2018). Site-specific incidence of ring-related injuries in Rufous fantails Rhipidura rufifrons. doi: 10.1080/03078698.2018.1505109
Pierce, Stevens, Mulder & Salewski (2007). Plastic colour rings and the incidence of leg injury in Flycatchers (Muscicapidae, Monarchidae). doi: 10.1080/03078698.2007.9674365
Schlich & Kempenaers (2018). The immediate impact of ringing, blood sampling and PIT-tag implanting on the behaviour of Blue tits Cyanistes caeruleus. doi: 10.5253/arde.v106i1.a8
Sedgwick & Klus (1997). Injury due to leg bands in Willow flycatchers. 
Song, Liu, Booksmythe & Ding (2017). Effects of individual-based preferences for colour-banded mates on sex allocation in Zebra finches. doi: 10.1093/beheco/arx069
Sutherland (2006). Ecological census techniques: a handbook (2nd ed.). New York, Cambridge University Press.
Weiser et al (2018). Effects of leg flags on nest survival of four species of Arctic-breeding shorebirds. doi: 10.1111/jofo.12264
Zann (1994). Effects of band color on survivorship, body condition and reproductive effort of free-living Australian Zebra finches. doi: 10.2307/4088512
}

\concept{tagging}
\concept{band}
\concept{marks}
\concept{ringing}
\concept{ecology methods}
\concept{field ecology}
\concept{animal behavior}
\concept{color tagging}


\examples{
genseq(30, 4,c("Black","Blue","Brown","Gray","Green"))
}
