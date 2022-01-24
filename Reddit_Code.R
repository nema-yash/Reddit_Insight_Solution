options(java.parameters = "-Xmx8000m")

#Libraries
library(syuzhet)
library(RedditExtractoR)
library(tidyr)
library(dplyr)
library(xlsx)
library(googlesheets)
library(RCurl)
library(httpuv)
library(stringr)
library(mailR)
library(RPostgreSQL)
library(RJDBC)
library(tidytext)
library(widyr)
library(rtrim)
library(aws.s3)

#Reading Googlesheet

Sys.setenv(AWS_ACCESS_KEY_ID = "****",
         AWS_SECRET_ACCESS_KEY = "******")


gs_auth(token)
myCsv <- gs_url("https://docs.google.com/spreadsheets/d/sheetkey/edit#gid=0")

data_main <- gs_read(myCsv)
rownum <- which(grepl("Yes", data_main$Insight_Flag)) +1
position <- paste("E",rownum,sep = "")
#str(data_main)
gs_edit_cells(myCsv, ws = "Sheet1", anchor = position[1], input = c("No"), byrow = FALSE)

# Variables to input data
#Reading Reddit Keyword and Start Date
data_main$Reddit_keyword[rownum[1]-1] <- paste0(data_main$Reddit_keyword[rownum[1]-1])
data_main$`Date(YYYY-MM-DD)`<- paste0(data_main$`Date(YYYY-MM-DD)`[rownum[1]-1])

reddit_links <- reddit_urls(
  search_terms   = data_main$Reddit_keyword[rownum[1]-1],
  page_threshold = 500
)

reddit_thread <- reddit_content(reddit_links$URL)

# Selecting relevant data from reddit_thread
reddit_thread_orig<-reddit_thread%>%
  select(id,structure,post_date,comm_date,num_comments,subreddit,upvote_prop,post_score,author,user,comment_score,controversiality,comment,title,domain)%>%
  mutate(post_day=weekdays(as.Date(post_date,"%d-%m-%y")),comment_day=weekdays(as.Date(comm_date,"%d-%m-%y")))

# Creating day name column for posts and comments
reddit_thread_orig$post_date<-as.Date(reddit_thread_orig$post_date,"%d-%m-%y")
reddit_thread_orig$comm_date<-as.Date(reddit_thread_orig$comm_date,"%d-%m-%y")

#Date filter
reddit_date_filter<-as.Date(data_main$`Date(YYYY-MM-DD)`[rownum[1]-1],format = "%Y-%m-%d")
reddit_date_filter_year<-substring(reddit_date_filter,1,4)
reddit_date_filtered<-paste0(as.character(as.numeric(reddit_date_filter_year)-1),substring(reddit_date_filter,5,10))


# Changing format of dates and  considering posts only from 1st June 2016
reddit_thread_orig$post_date<-as.POSIXct(reddit_thread_orig$post_date,format= "%d-%m-%y")
reddit_thread_orig$comm_date <-as.POSIXct(reddit_thread_orig$comm_date,format= "%d-%m-%y")
reddit_thread_orig<-reddit_thread_orig%>%
  filter(post_date>reddit_date_filtered)


reddit_thread_orig<-reddit_thread_orig%>%
  select(id,author,user,structure,post_date,post_day,comm_date,comment_day,subreddit,upvote_prop,post_score,num_comments,comment_score,controversiality,comment,title,domain)


#End of Analysis 1 # Start of Analysis 2
##################################Text analysis###############################################
###################################################Comment_Sentiments#####################################################################
reddit_thread_orig1<-reddit_thread_orig

fix.contractions <- function(doc) {
  # "won't" is a special case as it does not expand to "wo not"
  doc <- gsub("won't", "will not", doc)
  doc <- gsub("can't", "can not", doc)
  doc <- gsub("n't", " not", doc)
  doc <- gsub("'ll", " will", doc)
  doc <- gsub("'re", " are", doc)
  doc <- gsub("'ve", " have", doc)
  doc <- gsub("'m", " am", doc)
  doc <- gsub("'d", " would", doc)
  # 's could be 'is' or could be possessive: it has no expansion
  doc <- gsub("'s", "", doc)
  return(doc)
}

# fix (expand) contractions
reddit_thread_orig1$comment <- sapply(reddit_thread_orig1$comment, fix.contractions)

# function to remove special characters
removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]", "", x)
# remove special characters
reddit_thread_orig1$comment <- sapply(reddit_thread_orig1$comment, removeSpecialChars)

# convert everything to lower case
reddit_thread_orig1$comment <- sapply(reddit_thread_orig1$comment, tolower)

reddit_thread_orig1 <- reddit_thread_orig1 %>%
  mutate(comment_clean = str_replace_all(string = comment,
                                         c('\\babout\\b'='',	'\\babove\\b'='',	'\\bacross\\b'='',	'\\bafter\\b'='',	'\\bagain\\b'='',	'\\ball\\b'='',	'\\balmost\\b'='',	'\\balone\\b'='',	'\\balong\\b'='',	'\\balready\\b'='',	'\\balso\\b'='',	'\\balthough\\b'='',	'\\balways\\b'='',	'\\bamong\\b'='',	'\\ban\\b'='',	'\\band\\b'='',	'\\banother\\b'='',	'\\bany\\b'='',	'\\banybody\\b'='',	'\\banyone\\b'='',	'\\banything\\b'='',	'\\banywhere\\b'='',	'\\bare\\b'='',	'\\barea\\b'='',	'\\bareas\\b'='',	'\\baround\\b'='',	'\\bas\\b'='',	'\\bask\\b'='',	'\\basked\\b'='',	'\\basking\\b'='',	'\\basks\\b'='',	'\\bat\\b'='',	'\\baway\\b'='',	'\\bback\\b'='',	'\\bbacked\\b'='',	'\\bbacking\\b'='',	'\\bbacks\\b'='',	'\\bbe\\b'='',	'\\bbecame\\b'='',	'\\bbecause\\b'='',	'\\bbecome\\b'='',	'\\bbecomes\\b'='',	'\\bbeen\\b'='',	'\\bbefore\\b'='',	'\\bbegan\\b'='',	'\\bbehind\\b'='',	'\\bbeing\\b'='',	'\\bbeings\\b'='',	'\\bbetween\\b'='',	'\\bbig\\b'='',	'\\bboth\\b'='',	'\\bbut\\b'='',	'\\bby\\b'='',	'\\bcame\\b'='',	'\\bcan\\b'='',	'\\bcannot\\b'='',	'\\bcase\\b'='',	'\\bcases\\b'='',	'\\bcertain\\b'='',	'\\bcertainly\\b'='',	'\\bclear\\b'='',	'\\bclearly\\b'='',	'\\bcome\\b'='',	'\\bcould\\b'='',	'\\bdid\\b'='',	'\\bdiffer\\b'='',	'\\bdifferent\\b'='',	'\\bdifferently\\b'='',	'\\bdo\\b'='',	'\\bdoes\\b'='',	'\\bdone\\b'='',	'\\bdown\\b'='',	'\\bdowned\\b'='',	'\\bdowning\\b'='',	'\\bdowns\\b'='',	'\\bduring\\b'='',	'\\beach\\b'='',	'\\bearly\\b'='',	'\\beither\\b'='',	'\\bend\\b'='',	'\\bended\\b'='',	'\\bending\\b'='',	'\\bends\\b'='',	'\\benough\\b'='',	'\\beven\\b'='',	'\\bevenly\\b'='',	'\\bever\\b'='',	'\\bevery\\b'='',	'\\beverybody\\b'='',	'\\beveryone\\b'='',	'\\beverything\\b'='',	'\\beverywhere\\b'='',	'\\bface\\b'='',	'\\bfaces\\b'='',	'\\bfact\\b'='',	'\\bfacts\\b'='',	'\\bfar\\b'='',	'\\bfelt\\b'='',	'\\bfew\\b'='',	'\\bfind\\b'='',	'\\bfinds\\b'='',	'\\bfirst\\b'='',	'\\bfor\\b'='',	'\\bfour\\b'='',	'\\bfrom\\b'='',	'\\bfull\\b'='',	'\\bfully\\b'='',	'\\bfurther\\b'='',	'\\bfurthered\\b'='',	'\\bfurthering\\b'='',	'\\bfurthers\\b'='',	'\\bgave\\b'='',	'\\bgeneral\\b'='',	'\\bgenerally\\b'='',	'\\bget\\b'='',	'\\bgets\\b'='',	'\\bgive\\b'='',	'\\bgiven\\b'='',	'\\bgives\\b'='',	'\\bgo\\b'='',	'\\bgoing\\b'='',	'\\bgoods\\b'='',	'\\bgot\\b'='',	'\\bgroup\\b'='',	'\\bgrouped\\b'='',	'\\bgrouping\\b'='',	'\\bgroups\\b'='',	'\\bhad\\b'='',	'\\bhas\\b'='',	'\\bhave\\b'='',	'\\bhaving\\b'='',	'\\bhe\\b'='',	'\\bher\\b'='',	'\\bhere\\b'='',	'\\bherself\\b'='',	'\\bhim\\b'='',	'\\bhimself\\b'='',	'\\bhis\\b'='',	'\\bhow\\b'='',	'\\bhowever\\b'='',	'\\bif\\b'='',	'\\bimportant\\b'='',	'\\bin\\b'='',	'\\binterest\\b'='',	'\\binterested\\b'='',	'\\binteresting\\b'='',	'\\binterests\\b'='',	'\\binto\\b'='',	'\\bis\\b'='',	'\\bit\\b'='',	'\\bits\\b'='',	'\\bitself\\b'='',	'\\bjust\\b'='',	'\\bkeep\\b'='',	'\\bkeeps\\b'='',	'\\bkind\\b'='',	'\\bknew\\b'='',	'\\bknow\\b'='',	'\\bknown\\b'='',	'\\bknows\\b'='',	'\\blarge\\b'='',	'\\blargely\\b'='',	'\\blast\\b'='',	'\\blater\\b'='',	'\\blatest\\b'='',	'\\bleast\\b'='',	'\\bless\\b'='',	'\\blet\\b'='',	'\\blets\\b'='',	'\\blike\\b'='',	'\\blikely\\b'='',	'\\blong\\b'='',	'\\blonger\\b'='',	'\\blongest\\b'='',	'\\bmade\\b'='',	'\\bmake\\b'='',	'\\bmaking\\b'='',	'\\bman\\b'='',	'\\bmany\\b'='',	'\\bmay\\b'='',	'\\bme\\b'='',	'\\bmember\\b'='',	'\\bmembers\\b'='',	'\\bmen\\b'='',	'\\bmight\\b'='',	'\\bmore\\b'='',	'\\bmost\\b'='',	'\\bmostly\\b'='',	'\\bmr\\b'='',	'\\bmrs\\b'='',	'\\bmuch\\b'='',	'\\bmust\\b'='',	'\\bmy\\b'='',	'\\bmyself\\b'='',	'\\bnecessary\\b'='',	'\\bneed\\b'='',	'\\bneeded\\b'='',	'\\bneeding\\b'='',	'\\bneeds\\b'='','\\bnever\\b'='',	'\\bnew\\b'='')))%>%
  mutate(comment_clean = str_replace_all(string = comment, c('\\bnewer\\b'='',	'\\bnewest\\b'='',	'\\bno\\b'='',	'\\bnobody\\b'='',	'\\bnon\\b'='',	'\\bnoon\\b'='','\\bnot\\b'='',	'\\bnothing\\b'='',	'\\bnow\\b'='',	'\\bnowhere\\b'='',	'\\bnumber\\b'='',	'\\bnumbers\\b'='',	'\\bof\\b'='',	'\\boff\\b'='',	'\\boften\\b'='',	'\\bold\\b'='',	'\\bolder\\b'='',	'\\boldest\\b'='',	'\\bon\\b'='',	'\\bonce\\b'='',	'\\bone\\b'='',	'\\bonly\\b'='',	'\\bopen\\b'='',	'\\bopened\\b'='',	'\\bopening\\b'='',	'\\bopens\\b'='',	'\\bor\\b'='',	'\\border\\b'='',	'\\bordered\\b'='',	'\\bordering\\b'='',	'\\borders\\b'='',	'\\bother\\b'='',	'\\bothers\\b'='',	'\\bour\\b'='',	'\\bout\\b'='',	'\\bover\\b'='',	'\\bpart\\b'='',	'\\bparted\\b'='',	'\\bparting\\b'='',	'\\bparts\\b'='',	'\\bper\\b'='',	'\\bperhaps\\b'='',	'\\bplace\\b'='',	'\\bplaces\\b'='',	'\\bpoint\\b'='',	'\\bpointed\\b'='',	'\\bpointing\\b'='',	'\\bpoints\\b'='',	'\\bpossible\\b'='',	'\\bpresent\\b'='',	'\\bpresented\\b'='',	'\\bpresenting\\b'='',	'\\bpresents\\b'='',	'\\bputs\\b'='',	'\\bquite\\b'='',	'\\brather\\b'='',	'\\breally\\b'='',	'\\bright\\b'='',	'\\broom\\b'='',	'\\brooms\\b'='',	'\\bsaid\\b'='',	'\\bsame\\b'='',	'\\bsaw\\b'='',	'\\bsay\\b'='',	'\\bsays\\b'='',	'\\bsecond\\b'='',	'\\bseconds\\b'='',	'\\bsee\\b'='',	'\\bseem\\b'='',	'\\bseemed\\b'='',	'\\bseeming\\b'='',	'\\bseems\\b'='',	'\\bsees\\b'='',	'\\bseveral\\b'='',	'\\bshall\\b'='',	'\\bshe\\b'='',	'\\bshould\\b'='',	'\\bshow\\b'='',	'\\bshowed\\b'='',	'\\bshowing\\b'='',	'\\bshows\\b'='',	'\\bside\\b'='',	'\\bsides\\b'='',	'\\bsince\\b'='',	'\\bsmall\\b'='',	'\\bsmaller\\b'='',	'\\bsmallest\\b'='',	'\\bso\\b'='',	'\\bsome\\b'='',	'\\bsomebody\\b'='',	'\\bsomeone\\b'='',	'\\bsomething\\b'='',	'\\bsomewhere\\b'='',	'\\bstate\\b'='',	'\\bstates\\b'='',	'\\bstill\\b'='',	'\\bsuch\\b'='',	'\\bsure\\b'='',	'\\btake\\b'='',	'\\btaken\\b'='',	'\\bthan\\b'='',	'\\bthat\\b'='',	'\\bthe\\b'='',	'\\btheir\\b'='',	'\\bthem\\b'='',	'\\bthen\\b'='',	'\\bthere\\b'='',	'\\btherefore\\b'='',	'\\bthese\\b'='',	'\\bthey\\b'='',	'\\bthing\\b'='',	'\\bthings\\b'='',	'\\bthink\\b'='',	'\\bthinks\\b'='',	'\\bthis\\b'='',	'\\bthose\\b'='',	'\\bthough\\b'='',	'\\bthought\\b'='',	'\\bthoughts\\b'='',	'\\bthree\\b'='',	'\\bthrough\\b'='',	'\\bthus\\b'='',	'\\bto\\b'='',	'\\btoday\\b'='',	'\\btogether\\b'='',	'\\btoo\\b'='',	'\\btook\\b'='',	'\\btoward\\b'='',	'\\bturn\\b'='',	'\\bturned\\b'='',	'\\bturning\\b'='',	'\\bturns\\b'='',	'\\btwo\\b'='',	'\\bunder\\b'='',	'\\buntil\\b'='',	'\\bup\\b'='',	'\\bupon\\b'='',	'\\bus\\b'='',	'\\buse\\b'='',	'\\bused\\b'='',	'\\buses\\b'='',	'\\bvery\\b'='',	'\\bwant\\b'='',	'\\bwanted\\b'='',	'\\bwanting\\b'='',	'\\bwants\\b'='',	'\\bwas\\b'='',	'\\bway\\b'='',	'\\bways\\b'='',	'\\bwe\\b'='',	'\\bwell\\b'='',	'\\bwells\\b'='',	'\\bwent\\b'='',	'\\bwere\\b'='',	'\\bwhat\\b'='',	'\\bwhen\\b'='',	'\\bwhere\\b'='',	'\\bwhether\\b'='',	'\\bwhich\\b'='',	'\\bwhile\\b'='',	'\\bwho\\b'='',	'\\bwhole\\b'='',	'\\bwhose\\b'='',	'\\bwhy\\b'='',	'\\bwill\\b'='',	'\\bwith\\b'='',	'\\bwithin\\b'='',	'\\bwithout\\b'='',	'\\bwork\\b'='',	'\\bworked\\b'='',	'\\bworking\\b'='',	'\\bworks\\b'='',	'\\bwould\\b'='',	'\\byear\\b'='',	'\\byears\\b'='',	'\\byet\\b'='',	'\\byou\\b'='',	'\\byoung\\b'='',	'\\byounger\\b'='',	'\\byoungest\\b'='',	'\\byour\\b'='',	'\\byours\\b'='',	'\\byourself\\b'='',	'\\byourselves\\b'='','\\brepeat\\b'='' ,	'\\bwww\\b'='' ,	'\\bhttp\\b'='' ,	'\\bhttps\\b'='' ,	'\\bna\\b'='' ,	'\\btheres\\b'='' ,	'\\bbridge\\b'='' ,	'\\bfe0f\\b'='' ,	'\\byeah\\b'='' ,	'\\balright\\b'='' ,	'\\bwanna\\b'='' ,	'\\bgonna\\b'='' ,	'\\bchorus\\b'='' ,	'\\bwhoa\\b'='' ,	'\\bgotta\\b'='' ,	'\\bmake\\b'='' ,	'\\bmiscellaneous\\b'='' ,	'\\bbaby\\b'='' ,	'\\booh\\b'='' ,	'\\buurh\\b'='' ,	'\\bpheromone\\b'='' ,	'\\bmatic\\b'='' ,	'\\bai\\b'='' ,	'\\bca\\b'='' ,	'\\bla\\b'='' ,	'\\bda\\b'='' ,	'\\buh\\b'='' ,	'\\btin\\b'='' ,	'\\bll\\b'='' ,	'\\brepeats\\b'='' ,	'\\bla\\b'='' ,	'\\bda\\b'='' ,	'\\buh\\b'='' ,	'\\bverse\\b'='' ,	'\\bpoompoom\\b'='' ,	'\\bhey\\b'='' ,	'\\btranscription\\b'='', '\\bblyat\\b'='')))

reddit_thread_orig1$comment_clean<-trimws(reddit_thread_orig1$comment_clean, "l")
reddit_thread_orig1$comment_clean<-trimws(reddit_thread_orig1$comment_clean, "r")

reddit_thread_orig1$Length <- nchar(reddit_thread_orig1$comment_clean)
reddit_thread_orig2 <- reddit_thread_orig1 %>% filter(reddit_thread_orig1$Length >= 3)


Slangs_Removal <-
  c(
    '4r5e',
    '5h1t',
    'assfukka',
    'ballbag',
    'bi+ch',
    'blow job',
    'boooobs',
    'butthole',
    'cl1t',
    'cockmuncher',
    'cocksukka',
    'cums',
    'cuntlicking',
    'cyberfucking',
    'dlck',
    'ejaculates',
    'f4nny',
    'fannyflaps',
    'fellate',
    'fingerfucks',
    'fistfucks',
    'fuckheads',
    'fuckwit',
    'fukwit',
    'goatse',
    'hoar',
    'jerk-off',
    'knobjocky',
    'l3i+ch',
    'masochist',
    'masterbations',
    'mothafuckaz',
    'mothafucks',
    'motherfucking',
    'muther',
    'niggaz',
    'orgasim',
    'phuck',
    'piss',
    'porn',
    'pussy',
    'screwing',
    'shaggin',
    'shitfull',
    'shittings',
    'spunk',
    'tits',
    'turd',
    'vulva',
    'xxx',
    'miscarriage',
    '5hit',
    'asshole',
    'balls',
    'biatch',
    'blowjob',
    'booooobs',
    'buttmuch',
    'clit',
    'cocks',
    'cok',
    'cumshot',
    'cunts',
    'd1ck',
    'dog-fucker',
    'ejaculating',
    'fag',
    'fannyfucker',
    'fellatio',
    'fistfuck',
    'flange',
    'fuckin',
    'fudge packer',
    'fux',
    'God',
    'hoare',
    'jism',
    'knobjokey',
    'l3itch',
    'master-bate',
    'masturbate',
    'mothafucked',
    'mother fucker',
    'motherfuckings',
    'mutherfucker',
    'nigger',
    'orgasims',
    'phuk',
    'pissed',
    'porno',
    'pussys',
    'scroat',
    'shagging',
    'shithead',
    'shitty',
    's_h_i_t',
    'titt',
    'tw4t',
    'w00se',
    'moron',
    'a55',
    'assholes',
    'ballsack',
    'bitch',
    'blowjobs',
    'booooooobs',
    'buttplug',
    'clitoris',
    'cocksuck',
    'cokmuncher',
    'cunilingus',
    'cyalis',
    'damn',
    'doggin',
    'ejaculatings',
    'fagging',
    'fanyy',
    'fingerfuck',
    'fistfucked',
    'fook',
    'fucking',
    'fudgepacker',
    'fux0r',
    'god-dam',
    'hoer',
    'jiz',
    'kock',
    'labia',
    'masterb8',
    'mo-fo',
    'mothafucker',
    'motherfuck',
    'motherfuckka',
    'n1gga',
    'niggers',
    'orgasm',
    'phuked',
    'pisser',
    'pornography',
    'rectum',
    'scrote',
    'shemale',
    'shiting',
    'skank',
    't1tt1e5',
    'tittie5',
    'twat',
    'wang',
    'fuck off',
    'anal',
    'asswhole',
    'bastard',
    'bitcher',
    'boiolas',
    'breasts',
    'c0ck',
    'clits',
    'cocksucked',
    'coksucka',
    'cunillingus',
    'cyberfuc',
    'dick',
    'dogging',
    'ejaculation',
    'faggitt',
    'fatass',
    'fingerfucked',
    'fistfucker',
    'fooker',
    'fuckings',
    'fuk',
    'f_u_c_k',
    'god-damned',
    'homo',
    'jizm',
    'kondum',
    'lust',
    'masterbat*',
    'mof0',
    'mothafuckers',
    'motherfucked',
    'motherfucks',
    'n1gger',
    'nob',
    'orgasms',
    'phuking',
    'pissers',
    'pornos',
    'retard',
    'scrotum',
    'shi+',
    'shitings',
    'slut',
    't1tties',
    'tittiefucker',
    'twathead',
    'wank',
    'suck me',
    'anus',
    'a_s_s',
    'beastial',
    'bitchers',
    'bollock',
    'buceta',
    'c0cksucker',
    'cnut',
    'cocksucker',
    'coon',
    'cunnilingus',
    'cyberfuck',
    'dickhead',
    'donkeyribber',
    'ejakulate',
    'faggot',
    'fcuk',
    'fingerfucker',
    'fistfuckers',
    'fuck',
    'fuckingshitmotherfucker',
    'fuker',
    'gangbang',
    'goddamn',
    'hore',
    'jizz',
    'kondums',
    'lusting',
    'masterbat3',
    'mofo',
    'mothafuckin',
    'motherfucker',
    'muff',
    'nacked',
    'nob jokey',
    'p0rn',
    'phukked',
    'pisses',
    'prick',
    'rimjaw',
    'semen',
    'shit',
    'shits',
    'sluts',
    'teets',
    'titties',
    'twatty',
    'wanker',
    'motherfuck',
    'ar5e',
    'b!tch',
    'beastiality',
    'bitches',
    'bollok',
    'bugger',
    'carpet muncher',
    'cock',
    'cocksucking',
    'cox',
    'cunt',
    'cyberfucked',
    'dildo',
    'doosh',
    'f u c k',
    'faggs',
    'fcuker',
    'fingerfuckers',
    'fistfucking',
    'fucka',
    'fuckme',
    'fukker',
    'gangbanged',
    'goddamned',
    'horniest',
    'kawk',
    'kum',
    'm0f0',
    'masterbate',
    'mothafuck',
    'mothafucking',
    'motherfuckers',
    'mutha',
    'naked',
    'nobhead',
    'pawn',
    'phukking',
    'pissflaps',
    'pricks',
    'rimming',
    'sex',
    'shitdick',
    'shitted',
    'smegma',
    'teez',
    'tittyfuck',
    'twunt',
    'wanky',
    'brotherfucker',
    'arrse',
    'b00bs',
    'bellend',
    'bitchin',
    'boner',
    'bum',
    'cawk',
    'cock-sucker',
    'cocksucks',
    'crap',
    'cuntlick',
    'cyberfucker',
    'dildos',
    'duche',
    'fagot',
    'fcuking',
    'fucked',
    'fucks',
    'fukkin',
    'gangbangs',
    'hardcoresex',
    'horny',
    'knob',
    'kummer',
    'm0fo',
    'mothafucka',
    'muthafecker',
    'nazi',
    'nobjocky',
    'pecker',
    'phuks',
    'pissin',
    'pron',
    's hit',
    'sh!+',
    'shite',
    'shitter',
    'smut',
    'testical',
    'tittywank',
    'twunter',
    'whoar',
    'sisterfucker',
    'arse',
    'b17ch',
    'bestial',
    'bitching',
    'boob',
    'bunny fucker',
    'chink',
    'cockface',
    'cum',
    'dink',
    'dyke',
    'fagots',
    'feck',
    'fucker',
    'fuks',
    'gaylord',
    'hell',
    'hotsex',
    'knobead',
    'kumming',
    'm45terbate',
    'nigg3r',
    'nobjokey',
    'penis',
    'phuq',
    'pissing',
    'pube',
    's.o.b.',
    'sh!t',
    'shited',
    'shitters',
    'snatch',
    'testicle',
    'titwank',
    'v14gra',
    'whore',
    'ass',
    'boobs',
    'cockhead',
    'cummer',
    'dinks',
    'ejaculate',
    'fags',
    'fecker',
    'fuckers',
    'jack-off',
    'knobed',
    'kums',
    'ma5terb8',
    'nigg4h',
    'nude',
    'penisfucker',
    'pigfucker',
    'pissoff',
    'pusse',
    'sadist',
    'sh1t',
    'shitey',
    'son-of-a-bitch',
    'tit',
    'v1gra',
    'willies',
    'ass-fucker',
    'jackoff',
    'knobend',
    'nigga',
    'numbnuts',
    'pussi',
    'shag',
    'vagina',
    'willy',
    'asses',
    'niggah',
    'nudist',
    'nudism',
    'deleted'
  )

abc <- matrix(nrow = nrow(reddit_thread_orig2),ncol = length(Slangs_Removal))
for(i in 1:length(Slangs_Removal))
{
  abc[,i] <- grepl(Slangs_Removal[i],reddit_thread_orig2$comment_clean)
}

abc1 <- as.data.frame(abc)
abc1$sum <- rowSums(abc1)
reddit_thread_orig3 <- reddit_thread_orig2[abc1$sum == 0,]


reddit_thread_orig4<-reddit_thread_orig3%>%
  filter(!str_detect(as.character(comment_clean),"\\b.*[0-9].*\\b"))

reddit_thread_orig4$Length <- NULL

#Structure of reddit
reddit_thread_orig4$depth_comment <-str_count(reddit_thread_orig4$structure,"_")
reddit_thread_orig4$new_thread<-ifelse(reddit_thread_orig4$depth_comment==0,1,0)
reddit_thread_orig4$direct_comment<-ifelse(reddit_thread_orig4$depth_comment==1,1,0)
reddit_thread_orig4$comment_to_comment<-ifelse(reddit_thread_orig4$depth_comment>1,1,0)

# Normalizing comment_score between 0 to 1, creating index
# Preparing inputs for function min_max_normalization_func
max_score<-max(reddit_thread_orig4[,"comment_score"])
min_score<-min(reddit_thread_orig4[,"comment_score"])
min_max_normalization_func<-function(doc){
  doc<-as.numeric((doc-min_score)/(max_score-min_score))
  return(doc)
}

# Applying function
reddit_thread_orig4$comment_score_index <- sapply(reddit_thread_orig4$comment_score, min_max_normalization_func)

#Calculating the Sentiments of the Post
reddit_thread_orig4$sentiments <- get_nrc_sentiment(reddit_thread_orig4$comment_clean)
reddit_thread_orig4$P_N <- reddit_thread_orig4$sentiments[,10] - reddit_thread_orig4$sentiments[,9]
reddit_thread_orig4$Normalized_Index<-(10/(max(reddit_thread_orig4$P_N)- min(reddit_thread_orig4$P_N)))*((reddit_thread_orig4$P_N - max(reddit_thread_orig4$P_N)))+5

reddit_thread_orig4$sentiments_anger<- reddit_thread_orig4$sentiments[,1]
reddit_thread_orig4$sentiments_anticipation<- reddit_thread_orig4$sentiments[,2]
reddit_thread_orig4$sentiments_disgust<- reddit_thread_orig4$sentiments[,3]
reddit_thread_orig4$sentiments_fear<- reddit_thread_orig4$sentiments[,4]
reddit_thread_orig4$sentiments_joy<- reddit_thread_orig4$sentiments[,5]
reddit_thread_orig4$sentiments_sadness<- reddit_thread_orig4$sentiments[,6]
reddit_thread_orig4$sentiments_surprise<- reddit_thread_orig4$sentiments[,7]
reddit_thread_orig4$sentiments_trust<- reddit_thread_orig4$sentiments[,8]
reddit_thread_orig4$sentiments_negative<- reddit_thread_orig4$sentiments[,9]
reddit_thread_orig4$sentiments_positive<- reddit_thread_orig4$sentiments[,10]

reddit_thread_orig4$sentiments <- NULL
reddit_thread_orig4_1 <- reddit_thread_orig4[,c(1:23,26:35,24,25)]

reddit_thread_orig4_retain<-reddit_thread_orig4_1
names(reddit_thread_orig4_retain)[3]<-paste("User_Name")

reddit_thread_orig4_retain$Email_ID<-(data_main$Email[rownum[1]-1])

reddit_thread_data_final <- reddit_thread_orig4_retain
names(reddit_thread_orig4_retain) <- NULL

write.table(reddit_thread_orig4,zz,row.names = FALSE)

########################################################################################################################
#############################################WORD FREQUENCY#####################################################

reddit_thread_orig5<-reddit_thread_data_final

reddit_thread_orig5_retain<-reddit_thread_orig5%>%
  unnest_tokens(word, comment_clean)%>%
  filter(!nchar(word) < 3) %>%#Words like "ah" or "oo" used in music
  filter(!str_detect(as.character(word),"\\b.*[0-9].*\\b"))%>%#
  anti_join(stop_words)

reddit_thread_data_final2 <- reddit_thread_orig5_retain

names(reddit_thread_orig5_retain) <- NULL

write.csv(reddit_thread_orig5,"reddit_thread_orig5.csv")
