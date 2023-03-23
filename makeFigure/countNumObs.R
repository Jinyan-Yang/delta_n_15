all.df <- read.csv('cache/groundData.csv')
all.df <- all.df[!duplicated(all.df[,c('lon','lat','date','d15n')]),]

all.df$studyNm <- all.df$id
all.df$studyNm[grep('craine',all.df$studyNm)] <- 'craine'

summary(table(all.df$studyNm))

all.df.site <- all.df[!duplicated(all.df[,c('lon','lat')]),]
summary(table(all.df.site$studyNm))
