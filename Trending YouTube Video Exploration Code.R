setwd("C:/Users/huang/Desktop/678project")
library("ggplot2")
library(rjson)
library(jsonlite)
library(tidyverse)
library(RColorBrewer)
library(scales)

#Import data and data cleaning
us<-read.csv("US.csv",header = TRUE)
uscategory<-fromJSON("US_category_id.json")

ca<-read.csv("CA.csv",header = TRUE)
cacategory<-fromJSON("CA_category_id.json")

de<-read.csv("DE.csv",header = TRUE)
decategory<-fromJSON("DE_category_id.json")

fr<-read.csv("FR.csv",header = TRUE)
frcategory<-fromJSON("FR_category_id.json")

gb<-read.csv("GB.csv",header = TRUE)
gbcategory<-fromJSON("GB_category_id.json")

ind<-read.csv("IN.csv",header = TRUE)
indcategory<-fromJSON("IN_category_id.json")

jp<-read.csv("JP.csv",header = TRUE)
jpcategory<-fromJSON("JP_category_id.json")

kr<-read.csv("KR.csv",header = TRUE)
krcategory<-fromJSON("KR_category_id.json")

mx<-read.csv("MX.csv",header = TRUE)
mxcategory<-fromJSON("MX_category_id.json")

ru<-read.csv("RU.csv",header = TRUE)
rucategory<-fromJSON("RU_category_id.json")

category_id<-as.vector(uscategory$items$id)
category_name<-as.vector(uscategory$items$snippet$title)
uscategoryname<-as.data.frame(cbind(category_id,category_name))
uscategoryname$category_id<-as.numeric(levels(uscategoryname$category_id))[uscategoryname$category_id]
View(uscategoryname)
uss<-us%>%left_join(uscategoryname,by="category_id")

category_id<-as.vector(cacategory$items$id)
category_name<-as.vector(cacategory$items$snippet$title)
cacategoryname<-as.data.frame(cbind(category_id,category_name))
cacategoryname$category_id<-as.numeric(levels(cacategoryname$category_id))[cacategoryname$category_id]
cas<-ca%>%left_join(cacategoryname,by="category_id")
cas<-cas[,-c(9:34)]
cas<-cas[!is.na(cas$category_name),]
View(cas)

category_id<-as.vector(decategory$items$id)
category_name<-as.vector(decategory$items$snippet$title)
decategoryname<-as.data.frame(cbind(category_id,category_name))
decategoryname$category_id<-as.numeric(levels(decategoryname$category_id))[decategoryname$category_id]
des<-de%>%left_join(decategoryname,by="category_id")
des<-des[,-c(9:34)]
View(des)

category_id<-as.vector(frcategory$items$id)
category_name<-as.vector(frcategory$items$snippet$title)
frcategoryname<-as.data.frame(cbind(category_id,category_name))
frcategoryname$category_id<-as.numeric(levels(frcategoryname$category_id))[frcategoryname$category_id]
fr$category_id<-as.numeric(fr$category_id)
frs<-fr%>%left_join(frcategoryname,by="category_id")
frs<-frs[!is.na(frs$category_name),]
frs<-frs[,-c(9:47)]
View(frs)

category_id<-as.vector(gbcategory$items$id)
category_name<-as.vector(gbcategory$items$snippet$title)
gbcategoryname<-as.data.frame(cbind(category_id,category_name))
gbcategoryname$category_id<-as.numeric(levels(gbcategoryname$category_id))[gbcategoryname$category_id]
gbs<-gb%>%left_join(gbcategoryname,by="category_id")
gbs<-gbs[,-c(9:34)]
View(gbs)

category_id<-as.vector(indcategory$items$id)
category_name<-as.vector(indcategory$items$snippet$title)
indcategoryname<-as.data.frame(cbind(category_id,category_name))
indcategoryname$category_id<-as.numeric(levels(indcategoryname$category_id))[indcategoryname$category_id]
ind$category_id<-as.numeric(ind$category_id)
inds<-ind%>%left_join(indcategoryname,by="category_id")
inds<-inds[!is.na(inds$category_name),]
inds<-inds[,-c(9:26)]
View(inds)


category_id<-as.vector(jpcategory$items$id)
category_name<-as.vector(jpcategory$items$snippet$title)
jpcategoryname<-as.data.frame(cbind(category_id,category_name))
jpcategoryname$category_id<-as.numeric(levels(jpcategoryname$category_id))[jpcategoryname$category_id]
jp$category_id<-as.numeric(jp$category_id)
jps<-jp%>%left_join(jpcategoryname,by="category_id")
View(jps)

category_id<-as.vector(krcategory$items$id)
category_name<-as.vector(krcategory$items$snippet$title)
krcategoryname<-as.data.frame(cbind(category_id,category_name))
krcategoryname$category_id<-as.numeric(levels(krcategoryname$category_id))[krcategoryname$category_id]
kr$category_id<-as.numeric(kr$category_id)
krs<-kr%>%left_join(krcategoryname,by="category_id")
krs<-krs[,-c(9:41)]
View(krs)

category_id<-as.vector(mxcategory$items$id)
category_name<-as.vector(mxcategory$items$snippet$title)
mxcategoryname<-as.data.frame(cbind(category_id,category_name))
mxcategoryname$category_id<-as.numeric(levels(mxcategoryname$category_id))[mxcategoryname$category_id]
mx$category_id<-as.numeric(mx$category_id)
mxs<-mx%>%left_join(mxcategoryname,by="category_id")
mxs<-mxs[,-c(9:37)]
View(mxs)

category_id<-as.vector(rucategory$items$id)
category_name<-as.vector(rucategory$items$snippet$title)
rucategoryname<-as.data.frame(cbind(category_id,category_name))
rucategoryname$category_id<-as.numeric(levels(rucategoryname$category_id))[rucategoryname$category_id]
ru$category_id<-as.numeric(ru$category_id)
rus<-ru%>%left_join(rucategoryname,by="category_id")
rus<-rus[,-c(9:37)]
View(rus)

##EDA
#Views Top 5 by region
##us
category_view<-summarise(group_by(uss,category_name),sum(as.numeric(views)))
category_view<-category_view[order(category_view$`sum(as.numeric(views))`,decreasing = TRUE),]
category_view<-category_view[1:5,]
ggplot(category_view,aes(x=reorder(category_name,-category_view$`sum(as.numeric(views))`),y=category_view$`sum(as.numeric(views))`))+ geom_bar(stat = "identity",fill = brewer.pal(11, "RdBu")[2])+labs(title="United States")+
  labs(x="Category", y="Views")+theme_bw()+theme(panel.grid=element_blank(),panel.border=element_blank(),axis.line=element_line(size=1,colour="black"))+theme(plot.title = element_text(hjust = 0.5))+theme(axis.text.x=element_text(size=10))+theme(axis.text.y = element_blank())+theme(axis.ticks.y = element_blank()) 
ggsave("us_trending_views_bar_all.png",height = 10,width = 10)

##canada
category_view<-cas%>%group_by(category_name)%>%summarise(count=sum(as.numeric(views)))
category_view<-category_view[order((category_view$count),decreasing = TRUE),]
category_view<-category_view[1:5,]
ggplot(category_view,aes(x=reorder(category_name,-category_view$count),y=category_view$count))+ geom_bar(stat = "identity",fill = brewer.pal(11, "RdBu")[2])+labs(title="Canada")+
  labs(x="Category", y="Views")+theme_bw()+theme(panel.grid=element_blank(),panel.border=element_blank(),axis.line=element_line(size=1,colour="black"))+theme(plot.title = element_text(hjust = 0.5))+theme(axis.text.x=element_text(size=10))+theme(axis.text.y = element_blank())+theme(axis.ticks.y = element_blank()) 
ggsave("ca_trending_views_bar_all.png",height = 10,width = 10)

##Germany
category_view<-des%>%group_by(category_name)%>%summarise(count=sum(as.numeric(views)))
category_view<-category_view[order((category_view$count),decreasing = TRUE),]
category_view<-category_view[1:5,]
ggplot(category_view,aes(x=reorder(category_name,-category_view$count),y=category_view$count))+ geom_bar(stat = "identity",fill = brewer.pal(11, "RdBu")[2])+labs(title="Germany")+
  labs(x="Category", y="Views")+theme_bw()+theme(panel.grid=element_blank(),panel.border=element_blank(),axis.line=element_line(size=1,colour="black"))+theme(plot.title = element_text(hjust = 0.5))+theme(axis.text.x=element_text(size=10))+theme(axis.text.y = element_blank())+theme(axis.ticks.y = element_blank()) 
ggsave("de_trending_views_bar_all.png",height = 10,width = 10)

##France
category_view<-frs%>%group_by(category_name)%>%summarise(count=sum(as.numeric(views)))
category_view<-category_view[order((category_view$count),decreasing = TRUE),]
category_view<-category_view[1:5,]
ggplot(category_view,aes(x=reorder(category_name,-category_view$count),y=category_view$count))+ geom_bar(stat = "identity",fill = brewer.pal(11, "RdBu")[2])+labs(title="France")+
  labs(x="Category", y="Views")+theme_bw()+theme(panel.grid=element_blank(),panel.border=element_blank(),axis.line=element_line(size=1,colour="black"))+theme(plot.title = element_text(hjust = 0.5))+theme(axis.text.x=element_text(size=10))+theme(axis.text.y = element_blank())+theme(axis.ticks.y = element_blank()) 
ggsave("fr_trending_views_bar_all.png",height = 10,width = 10)

##Great Britain
category_view<-gbs%>%group_by(category_name)%>%summarise(count=sum(as.numeric(views)))
category_view<-category_view[order((category_view$count),decreasing = TRUE),]
category_view<-category_view[-13,]
category_view<-category_view[1:5,]
ggplot(category_view,aes(x=reorder(category_name,-category_view$count),y=category_view$count))+ geom_bar(stat = "identity",fill = brewer.pal(11, "RdBu")[2])+labs(title="Great Britain")+
  labs(x="Category", y="Views")+theme_bw()+theme(panel.grid=element_blank(),panel.border=element_blank(),axis.line=element_line(size=1,colour="black"))+theme(plot.title = element_text(hjust = 0.5))+theme(axis.text.x=element_text(size=10))+theme(axis.text.y = element_blank())+theme(axis.ticks.y = element_blank()) 
ggsave("gb_trending_views_bar_all.png",height = 10,width = 10)


##India
category_view<-inds%>%group_by(category_name)%>%summarise(count=sum(as.numeric(views)))
category_view<-category_view[order((category_view$count),decreasing = TRUE),]
category_view<-category_view[1:5,]
ggplot(category_view,aes(x=reorder(category_name,-category_view$count),y=category_view$count))+ geom_bar(stat = "identity",fill = brewer.pal(11, "RdBu")[2])+labs(title="India")+
  labs(x="Category", y="Views")+theme_bw()+theme(panel.grid=element_blank(),panel.border=element_blank(),axis.line=element_line(size=1,colour="black"))+theme(plot.title = element_text(hjust = 0.5))+theme(axis.text.x=element_text(size=10))+theme(axis.text.y = element_blank())+theme(axis.ticks.y = element_blank()) 
ggsave("ind_trending_views_bar_all.png",height = 10,width = 10)


##Japan
category_view<-jps%>%group_by(category_name)%>%summarise(count=sum(as.numeric(views)))
category_view<-category_view[order((category_view$count),decreasing = TRUE),]
category_view<-category_view[-15,]
category_view<-category_view[1:5,]
ggplot(category_view,aes(x=reorder(category_name,-category_view$count),y=category_view$count))+ geom_bar(stat = "identity",fill = brewer.pal(11, "RdBu")[2])+labs(title="Japan")+
  labs(x="Category", y="Views")+theme_bw()+theme(panel.grid=element_blank(),panel.border=element_blank(),axis.line=element_line(size=1,colour="black"))+theme(plot.title = element_text(hjust = 0.5))+theme(axis.text.x=element_text(size=10))+theme(axis.text.y = element_blank())+theme(axis.ticks.y = element_blank()) 
ggsave("jp_trending_views_bar_all.png",height = 10,width = 10)


##Korea
category_view<-krs%>%group_by(category_name)%>%summarise(count=sum(as.numeric(views)))
category_view<-category_view[order((category_view$count),decreasing = TRUE),]
category_view<-category_view[-13,]
category_view<-category_view[1:5,]
ggplot(category_view,aes(x=reorder(category_name,-category_view$count),y=category_view$count))+ geom_bar(stat = "identity",fill = brewer.pal(11, "RdBu")[2])+labs(title="Korea")+
  labs(x="Category", y="Views")+theme_bw()+theme(panel.grid=element_blank(),panel.border=element_blank(),axis.line=element_line(size=1,colour="black"))+theme(plot.title = element_text(hjust = 0.5))+theme(axis.text.x=element_text(size=10))+theme(axis.text.y = element_blank())+theme(axis.ticks.y = element_blank()) 
ggsave("kr_trending_views_bar_all.png",height = 10,width = 10)

##Mexico
category_view<-mxs%>%group_by(category_name)%>%summarise(count=sum(as.numeric(views)))
category_view<-category_view[order((category_view$count),decreasing = TRUE),]
category_view<-category_view[-13,]
category_view<-category_view[1:5,]
ggplot(category_view,aes(x=reorder(category_name,-category_view$count),y=category_view$count))+ geom_bar(stat = "identity",fill = brewer.pal(11, "RdBu")[2])+labs(title="Mexico")+
  labs(x="Category", y="Views")+theme_bw()+theme(panel.grid=element_blank(),panel.border=element_blank(),axis.line=element_line(size=1,colour="black"))+theme(plot.title = element_text(hjust = 0.5))+theme(axis.text.x=element_text(size=10))+theme(axis.text.y = element_blank())+theme(axis.ticks.y = element_blank()) 
ggsave("mx_trending_views_bar_all.png",height = 10,width = 10)


##Russia
category_view<-rus%>%group_by(category_name)%>%summarise(count=sum(as.numeric(views)))
category_view<-category_view[order((category_view$count),decreasing = TRUE),]
category_view<-category_view[-11,]
category_view<-category_view[1:5,]
ggplot(category_view,aes(x=reorder(category_name,-category_view$count),y=category_view$count))+ geom_bar(stat = "identity",fill = brewer.pal(11, "RdBu")[2])+labs(title="Russia")+
  labs(x="Category", y="Views")+theme_bw()+theme(panel.grid=element_blank(),panel.border=element_blank(),axis.line=element_line(size=1,colour="black"))+theme(plot.title = element_text(hjust = 0.5))+theme(axis.text.x=element_text(size=10))+theme(axis.text.y = element_blank())+theme(axis.ticks.y = element_blank()) 
ggsave("ru_trending_views_bar_all.png",height = 10,width = 10)

#Likes and dislikes Top 5 by region
##us
category_likes<-summarise(group_by(uss,category_name),sum(as.numeric(likes)))
category_dislikes<-summarise(group_by(uss,category_name),sum(as.numeric(dislikes)))
likerate<-category_likes%>%left_join(category_dislikes,by="category_name")
likerate<-mutate(likerate,rate=percent(likerate$`sum(as.numeric(likes))`/(likerate$`sum(as.numeric(likes))`+likerate$`sum(as.numeric(dislikes))`),digits = 2,sep=""),ratee=likerate$`sum(as.numeric(likes))`/(likerate$`sum(as.numeric(likes))`+likerate$`sum(as.numeric(dislikes))`))
colnames(likerate)[2]<-"likes"
colnames(likerate)[3]<-"dislikes"
likerate<-mutate(likerate,ld<-likes+dislikes)
colnames(likerate)[6]<-"ld"
ld<-likerate%>%gather(likes,dislikes,key="Attitude",value="clicks")
ld<-ld[order(ld$ld,decreasing = TRUE),]
ld<-ld[1:10,]
ggplot(ld,mapping = aes(x=reorder(category_name,-ld),clicks,fill=Attitude))+geom_bar(stat="identity",position='stack')+labs(title="United States")+theme_bw()+theme(panel.grid=element_blank(),panel.border=element_blank(),axis.line=element_line(size=1,colour="black"))+labs(x="Category", y="Likes and Dislikes")+theme(axis.text.x=element_text(size=10))+theme(axis.text.y = element_blank())+theme(axis.ticks.y = element_blank())+scale_fill_manual(values=c('black', brewer.pal(11, "RdBu")[2]))+theme(plot.title = element_text(hjust = 0.5))
ggsave("us_trending_likes_dislikes_sum_bar_all.png",height = 10,width = 10)

##canada
category_likes<-summarise(group_by(cas,category_name),sum(as.numeric(likes)))
category_dislikes<-summarise(group_by(cas,category_name),sum(as.numeric(dislikes)))
likerate<-category_likes%>%left_join(category_dislikes,by="category_name")
likerate<-mutate(likerate,rate=percent(likerate$`sum(as.numeric(likes))`/(likerate$`sum(as.numeric(likes))`+likerate$`sum(as.numeric(dislikes))`),digits = 2,sep=""),ratee=likerate$`sum(as.numeric(likes))`/(likerate$`sum(as.numeric(likes))`+likerate$`sum(as.numeric(dislikes))`))
colnames(likerate)[2]<-"likes"
colnames(likerate)[3]<-"dislikes"
likerate<-mutate(likerate,ld<-likes+dislikes)
colnames(likerate)[6]<-"ld"
ld<-likerate%>%gather(likes,dislikes,key="Attitude",value="clicks")
ld<-ld[order(ld$ld,decreasing = TRUE),]
ld<-ld[1:10,]
ggplot(ld,mapping = aes(x=reorder(category_name,-ld),clicks,fill=Attitude))+geom_bar(stat="identity",position='stack')+labs(title="Canada")+theme_bw()+theme(panel.grid=element_blank(),panel.border=element_blank(),axis.line=element_line(size=1,colour="black"))+labs(x="Category", y="Likes and Dislikes")+theme(axis.text.x=element_text(size=10))+theme(axis.text.y = element_blank())+theme(axis.ticks.y = element_blank())+scale_fill_manual(values=c('black', brewer.pal(11, "RdBu")[2]))+theme(plot.title = element_text(hjust = 0.5))
ggsave("ca_trending_likes_dislikes_sum_bar_all.png",height = 10,width = 10)

##Germany
category_likes<-summarise(group_by(des,category_name),sum(as.numeric(likes)))
category_dislikes<-summarise(group_by(des,category_name),sum(as.numeric(dislikes)))
likerate<-category_likes%>%left_join(category_dislikes,by="category_name")
likerate<-mutate(likerate,rate=percent(likerate$`sum(as.numeric(likes))`/(likerate$`sum(as.numeric(likes))`+likerate$`sum(as.numeric(dislikes))`),digits = 2,sep=""),ratee=likerate$`sum(as.numeric(likes))`/(likerate$`sum(as.numeric(likes))`+likerate$`sum(as.numeric(dislikes))`))
colnames(likerate)[2]<-"likes"
colnames(likerate)[3]<-"dislikes"
likerate<-mutate(likerate,ld<-likes+dislikes)
colnames(likerate)[6]<-"ld"
ld<-likerate%>%gather(likes,dislikes,key="Attitude",value="clicks")
ld<-ld[order(ld$ld,decreasing = TRUE),]
ld<-ld[1:10,]
ggplot(ld,mapping = aes(x=reorder(category_name,-ld),clicks,fill=Attitude))+geom_bar(stat="identity",position='stack')+labs(title="Germany")+theme_bw()+theme(panel.grid=element_blank(),panel.border=element_blank(),axis.line=element_line(size=1,colour="black"))+labs(x="Category", y="Likes and Dislikes")+theme(axis.text.x=element_text(size=10))+theme(axis.text.y = element_blank())+theme(axis.ticks.y = element_blank())+scale_fill_manual(values=c('black', brewer.pal(11, "RdBu")[2]))+theme(plot.title = element_text(hjust = 0.5))
ggsave("de_trending_likes_dislikes_sum_bar_all.png",height = 10,width = 10)

##France
category_likes<-summarise(group_by(frs,category_name),sum(as.numeric(likes)))
category_dislikes<-summarise(group_by(frs,category_name),sum(as.numeric(dislikes)))
likerate<-category_likes%>%left_join(category_dislikes,by="category_name")
likerate<-mutate(likerate,rate=percent(likerate$`sum(as.numeric(likes))`/(likerate$`sum(as.numeric(likes))`+likerate$`sum(as.numeric(dislikes))`),digits = 2,sep=""),ratee=likerate$`sum(as.numeric(likes))`/(likerate$`sum(as.numeric(likes))`+likerate$`sum(as.numeric(dislikes))`))
colnames(likerate)[2]<-"likes"
colnames(likerate)[3]<-"dislikes"
likerate<-mutate(likerate,ld<-likes+dislikes)
colnames(likerate)[6]<-"ld"
ld<-likerate%>%gather(likes,dislikes,key="Attitude",value="clicks")
ld<-ld[order(ld$ld,decreasing = TRUE),]
ld<-ld[1:10,]
ggplot(ld,mapping = aes(x=reorder(category_name,-ld),clicks,fill=Attitude))+geom_bar(stat="identity",position='stack')+labs(title="France")+theme_bw()+theme(panel.grid=element_blank(),panel.border=element_blank(),axis.line=element_line(size=1,colour="black"))+labs(x="Category", y="Likes and Dislikes")+theme(axis.text.x=element_text(size=10))+theme(axis.text.y = element_blank())+theme(axis.ticks.y = element_blank())+scale_fill_manual(values=c('black', brewer.pal(11, "RdBu")[2]))+theme(plot.title = element_text(hjust = 0.5))
ggsave("fr_trending_likes_dislikes_sum_bar_all.png",height = 10,width = 10)

##United Kindom
category_likes<-summarise(group_by(gbs,category_name),sum(as.numeric(likes)))
category_dislikes<-summarise(group_by(gbs,category_name),sum(as.numeric(dislikes)))
likerate<-category_likes%>%left_join(category_dislikes,by="category_name")
likerate<-mutate(likerate,rate=percent(likerate$`sum(as.numeric(likes))`/(likerate$`sum(as.numeric(likes))`+likerate$`sum(as.numeric(dislikes))`),digits = 2,sep=""),ratee=likerate$`sum(as.numeric(likes))`/(likerate$`sum(as.numeric(likes))`+likerate$`sum(as.numeric(dislikes))`))
colnames(likerate)[2]<-"likes"
colnames(likerate)[3]<-"dislikes"
likerate<-mutate(likerate,ld<-likes+dislikes)
colnames(likerate)[6]<-"ld"
ld<-likerate%>%gather(likes,dislikes,key="Attitude",value="clicks")
ld<-ld[order(ld$ld,decreasing = TRUE),]
ld<-ld[1:10,]
ggplot(ld,mapping = aes(x=reorder(category_name,-ld),clicks,fill=Attitude))+geom_bar(stat="identity",position='stack')+labs(title="United Kindom")+theme_bw()+theme(panel.grid=element_blank(),panel.border=element_blank(),axis.line=element_line(size=1,colour="black"))+labs(x="Category", y="Likes and Dislikes")+theme(axis.text.x=element_text(size=10))+theme(axis.text.y = element_blank())+theme(axis.ticks.y = element_blank())+scale_fill_manual(values=c('black', brewer.pal(11, "RdBu")[2]))+theme(plot.title = element_text(hjust = 0.5))
ggsave("gb_trending_likes_dislikes_sum_bar_all.png",height = 10,width = 10)

##India
category_likes<-summarise(group_by(inds,category_name),sum(as.numeric(likes)))
category_dislikes<-summarise(group_by(inds,category_name),sum(as.numeric(dislikes)))
likerate<-category_likes%>%left_join(category_dislikes,by="category_name")
likerate<-mutate(likerate,rate=percent(likerate$`sum(as.numeric(likes))`/(likerate$`sum(as.numeric(likes))`+likerate$`sum(as.numeric(dislikes))`),digits = 2,sep=""),ratee=likerate$`sum(as.numeric(likes))`/(likerate$`sum(as.numeric(likes))`+likerate$`sum(as.numeric(dislikes))`))
colnames(likerate)[2]<-"likes"
colnames(likerate)[3]<-"dislikes"
likerate<-mutate(likerate,ld<-likes+dislikes)
colnames(likerate)[6]<-"ld"
ld<-likerate%>%gather(likes,dislikes,key="Attitude",value="clicks")
ld<-ld[order(ld$ld,decreasing = TRUE),]
ld<-ld[1:10,]
ggplot(ld,mapping = aes(x=reorder(category_name,-ld),clicks,fill=Attitude))+geom_bar(stat="identity",position='stack')+labs(title="India")+theme_bw()+theme(panel.grid=element_blank(),panel.border=element_blank(),axis.line=element_line(size=1,colour="black"))+labs(x="Category", y="Likes and Dislikes")+theme(axis.text.x=element_text(size=10))+theme(axis.text.y = element_blank())+theme(axis.ticks.y = element_blank())+scale_fill_manual(values=c('black', brewer.pal(11, "RdBu")[2]))+theme(plot.title = element_text(hjust = 0.5))
ggsave("ind_trending_likes_dislikes_sum_bar_all.png",height = 10,width = 10)


##Japan
category_likes<-summarise(group_by(jps,category_name),sum(as.numeric(likes)))
category_dislikes<-summarise(group_by(jps,category_name),sum(as.numeric(dislikes)))
likerate<-category_likes%>%left_join(category_dislikes,by="category_name")
likerate<-mutate(likerate,rate=percent(likerate$`sum(as.numeric(likes))`/(likerate$`sum(as.numeric(likes))`+likerate$`sum(as.numeric(dislikes))`),digits = 2,sep=""),ratee=likerate$`sum(as.numeric(likes))`/(likerate$`sum(as.numeric(likes))`+likerate$`sum(as.numeric(dislikes))`))
colnames(likerate)[2]<-"likes"
colnames(likerate)[3]<-"dislikes"
likerate<-mutate(likerate,ld<-likes+dislikes)
colnames(likerate)[6]<-"ld"
ld<-likerate%>%gather(likes,dislikes,key="Attitude",value="clicks")
ld<-ld[order(ld$ld,decreasing = TRUE),]
ld<-ld[1:10,]
ggplot(ld,mapping = aes(x=reorder(category_name,-ld),clicks,fill=Attitude))+geom_bar(stat="identity",position='stack')+labs(title="Japan")+theme_bw()+theme(panel.grid=element_blank(),panel.border=element_blank(),axis.line=element_line(size=1,colour="black"))+labs(x="Category", y="Likes and Dislikes")+theme(axis.text.x=element_text(size=10))+theme(axis.text.y = element_blank())+theme(axis.ticks.y = element_blank())+scale_fill_manual(values=c('black', brewer.pal(11, "RdBu")[2]))+theme(plot.title = element_text(hjust = 0.5))
ggsave("jp_trending_likes_dislikes_sum_bar_all.png",height = 10,width = 10)


##Korea
category_likes<-summarise(group_by(krs,category_name),sum(as.numeric(likes)))
category_dislikes<-summarise(group_by(krs,category_name),sum(as.numeric(dislikes)))
likerate<-category_likes%>%left_join(category_dislikes,by="category_name")
likerate<-mutate(likerate,rate=percent(likerate$`sum(as.numeric(likes))`/(likerate$`sum(as.numeric(likes))`+likerate$`sum(as.numeric(dislikes))`),digits = 2,sep=""),ratee=likerate$`sum(as.numeric(likes))`/(likerate$`sum(as.numeric(likes))`+likerate$`sum(as.numeric(dislikes))`))
colnames(likerate)[2]<-"likes"
colnames(likerate)[3]<-"dislikes"
likerate<-mutate(likerate,ld<-likes+dislikes)
colnames(likerate)[6]<-"ld"
ld<-likerate%>%gather(likes,dislikes,key="Attitude",value="clicks")
ld<-ld[order(ld$ld,decreasing = TRUE),]
ld<-ld[1:10,]
ggplot(ld,mapping = aes(x=reorder(category_name,-ld),clicks,fill=Attitude))+geom_bar(stat="identity",position='stack')+labs(title="Korea")+theme_bw()+theme(panel.grid=element_blank(),panel.border=element_blank(),axis.line=element_line(size=1,colour="black"))+labs(x="Category", y="Likes and Dislikes")+theme(axis.text.x=element_text(size=10))+theme(axis.text.y = element_blank())+theme(axis.ticks.y = element_blank())+scale_fill_manual(values=c('black', brewer.pal(11, "RdBu")[2]))+theme(plot.title = element_text(hjust = 0.5))
ggsave("kr_trending_likes_dislikes_sum_bar_all.png",height = 10,width = 10)


##Mexico
category_likes<-summarise(group_by(mxs,category_name),sum(as.numeric(likes)))
category_dislikes<-summarise(group_by(mxs,category_name),sum(as.numeric(dislikes)))
likerate<-category_likes%>%left_join(category_dislikes,by="category_name")
likerate<-mutate(likerate,rate=percent(likerate$`sum(as.numeric(likes))`/(likerate$`sum(as.numeric(likes))`+likerate$`sum(as.numeric(dislikes))`),digits = 2,sep=""),ratee=likerate$`sum(as.numeric(likes))`/(likerate$`sum(as.numeric(likes))`+likerate$`sum(as.numeric(dislikes))`))
colnames(likerate)[2]<-"likes"
colnames(likerate)[3]<-"dislikes"
likerate<-mutate(likerate,ld<-likes+dislikes)
colnames(likerate)[6]<-"ld"
ld<-likerate%>%gather(likes,dislikes,key="Attitude",value="clicks")
ld<-ld[order(ld$ld,decreasing = TRUE),]
ld<-ld[1:10,]
ggplot(ld,mapping = aes(x=reorder(category_name,-ld),clicks,fill=Attitude))+geom_bar(stat="identity",position='stack')+labs(title="Mexico")+theme_bw()+theme(panel.grid=element_blank(),panel.border=element_blank(),axis.line=element_line(size=1,colour="black"))+labs(x="Category", y="Likes and Dislikes")+theme(axis.text.x=element_text(size=10))+theme(axis.text.y = element_blank())+theme(axis.ticks.y = element_blank())+scale_fill_manual(values=c('black', brewer.pal(11, "RdBu")[2]))+theme(plot.title = element_text(hjust = 0.5))
ggsave("mx_trending_likes_dislikes_sum_bar_all.png",height = 10,width = 10)


##Russia
category_likes<-summarise(group_by(rus,category_name),sum(as.numeric(likes)))
category_dislikes<-summarise(group_by(rus,category_name),sum(as.numeric(dislikes)))
likerate<-category_likes%>%left_join(category_dislikes,by="category_name")
likerate<-mutate(likerate,rate=percent(likerate$`sum(as.numeric(likes))`/(likerate$`sum(as.numeric(likes))`+likerate$`sum(as.numeric(dislikes))`),digits = 2,sep=""),ratee=likerate$`sum(as.numeric(likes))`/(likerate$`sum(as.numeric(likes))`+likerate$`sum(as.numeric(dislikes))`))
colnames(likerate)[2]<-"likes"
colnames(likerate)[3]<-"dislikes"
likerate<-mutate(likerate,ld<-likes+dislikes)
colnames(likerate)[6]<-"ld"
ld<-likerate%>%gather(likes,dislikes,key="Attitude",value="clicks")
ld<-ld[order(ld$ld,decreasing = TRUE),]
ld<-ld[1:10,]
ggplot(ld,mapping = aes(x=reorder(category_name,-ld),clicks,fill=Attitude))+geom_bar(stat="identity",position='stack')+labs(title="Russia")+theme_bw()+theme(panel.grid=element_blank(),panel.border=element_blank(),axis.line=element_line(size=1,colour="black"))+labs(x="Category", y="Likes and Dislikes")+theme(axis.text.x=element_text(size=10))+theme(axis.text.y = element_blank())+theme(axis.ticks.y = element_blank())+scale_fill_manual(values=c('black', brewer.pal(11, "RdBu")[2]))+theme(plot.title = element_text(hjust = 0.5))
ggsave("ru_trending_likes_dislikes_sum_bar_all.png",height = 10,width = 10)

#Comments Top 5 by region
##us
category_comments<-summarise(group_by(uss,category_name),sum(as.numeric(comment_count)))
category_comments<-category_comments[order(category_comments$`sum(as.numeric(comment_count))`,decreasing = TRUE),]
category_comments<-category_comments[1:5,]
ggplot(category_comments,aes(x=reorder(category_name,-category_comments$`sum(as.numeric(comment_count))`),y=category_comments$`sum(as.numeric(comment_count))`))+ geom_bar(stat = "identity",fill = brewer.pal(11, "RdBu")[2])+labs(title="United States")+
  labs(x="Category", y="Comments")+theme_bw()+theme(panel.grid=element_blank(),panel.border=element_blank(),axis.line=element_line(size=1,colour="black"))+theme(plot.title = element_text(hjust = 0.5))+theme(axis.text.x=element_text(size=10))+theme(axis.text.y = element_blank())+theme(axis.ticks.y = element_blank())
ggsave("us_trending_comments_bar_all.png",height = 10,width = 10)

##Canada
category_comments<-summarise(group_by(cas,category_name),sum(as.numeric(comment_count)))
category_comments<-category_comments[order(category_comments$`sum(as.numeric(comment_count))`,decreasing = TRUE),]
category_comments<-category_comments[1:5,]
ggplot(category_comments,aes(x=reorder(category_name,-category_comments$`sum(as.numeric(comment_count))`),y=category_comments$`sum(as.numeric(comment_count))`))+ geom_bar(stat = "identity",fill = brewer.pal(11, "RdBu")[2])+labs(title="Canada")+
  labs(x="Category", y="Comments")+theme_bw()+theme(panel.grid=element_blank(),panel.border=element_blank(),axis.line=element_line(size=1,colour="black"))+theme(plot.title = element_text(hjust = 0.5))+theme(axis.text.x=element_text(size=10))+theme(axis.text.y = element_blank())+theme(axis.ticks.y = element_blank())
ggsave("cas_trending_comments_bar_all.png",height = 10,width = 10)

##Germany
category_comments<-summarise(group_by(des,category_name),sum(as.numeric(comment_count)))
category_comments<-category_comments[order(category_comments$`sum(as.numeric(comment_count))`,decreasing = TRUE),]
category_comments<-category_comments[1:5,]
ggplot(category_comments,aes(x=reorder(category_name,-category_comments$`sum(as.numeric(comment_count))`),y=category_comments$`sum(as.numeric(comment_count))`))+ geom_bar(stat = "identity",fill = brewer.pal(11, "RdBu")[2])+labs(title="Germany")+
  labs(x="Category", y="Comments")+theme_bw()+theme(panel.grid=element_blank(),panel.border=element_blank(),axis.line=element_line(size=1,colour="black"))+theme(plot.title = element_text(hjust = 0.5))+theme(axis.text.x=element_text(size=10))+theme(axis.text.y = element_blank())+theme(axis.ticks.y = element_blank())
ggsave("de_trending_comments_bar_all.png",height = 10,width = 10)

##France
category_comments<-summarise(group_by(frs,category_name),sum(as.numeric(comment_count)))
category_comments<-category_comments[order(category_comments$`sum(as.numeric(comment_count))`,decreasing = TRUE),]
category_comments<-category_comments[1:5,]
ggplot(category_comments,aes(x=reorder(category_name,-category_comments$`sum(as.numeric(comment_count))`),y=category_comments$`sum(as.numeric(comment_count))`))+ geom_bar(stat = "identity",fill = brewer.pal(11, "RdBu")[2])+labs(title="France")+
  labs(x="Category", y="Comments")+theme_bw()+theme(panel.grid=element_blank(),panel.border=element_blank(),axis.line=element_line(size=1,colour="black"))+theme(plot.title = element_text(hjust = 0.5))+theme(axis.text.x=element_text(size=10))+theme(axis.text.y = element_blank())+theme(axis.ticks.y = element_blank())
ggsave("fr_trending_comments_bar_all.png",height = 10,width = 10)

##Great Britain
category_comments<-summarise(group_by(gbs,category_name),sum(as.numeric(comment_count)))
category_comments<-category_comments[order(category_comments$`sum(as.numeric(comment_count))`,decreasing = TRUE),]
category_comments<-category_comments[1:5,]
ggplot(category_comments,aes(x=reorder(category_name,-category_comments$`sum(as.numeric(comment_count))`),y=category_comments$`sum(as.numeric(comment_count))`))+ geom_bar(stat = "identity",fill = brewer.pal(11, "RdBu")[2])+labs(title="United Kingdom")+
  labs(x="Category", y="Comments")+theme_bw()+theme(panel.grid=element_blank(),panel.border=element_blank(),axis.line=element_line(size=1,colour="black"))+theme(plot.title = element_text(hjust = 0.5))+theme(axis.text.x=element_text(size=10))+theme(axis.text.y = element_blank())+theme(axis.ticks.y = element_blank())
ggsave("gb_trending_comments_bar_all.png",height = 10,width = 10)

##India
category_comments<-summarise(group_by(inds,category_name),sum(as.numeric(comment_count)))
category_comments<-category_comments[order(category_comments$`sum(as.numeric(comment_count))`,decreasing = TRUE),]
category_comments<-category_comments[1:5,]
ggplot(category_comments,aes(x=reorder(category_name,-category_comments$`sum(as.numeric(comment_count))`),y=category_comments$`sum(as.numeric(comment_count))`))+ geom_bar(stat = "identity",fill = brewer.pal(11, "RdBu")[2])+labs(title="India")+
  labs(x="Category", y="Comments")+theme_bw()+theme(panel.grid=element_blank(),panel.border=element_blank(),axis.line=element_line(size=1,colour="black"))+theme(plot.title = element_text(hjust = 0.5))+theme(axis.text.x=element_text(size=10))+theme(axis.text.y = element_blank())+theme(axis.ticks.y = element_blank())
ggsave("ind_trending_comments_bar_all.png",height = 10,width = 10)


##Japan
category_comments<-summarise(group_by(jps,category_name),sum(as.numeric(comment_count)))
category_comments<-category_comments[order(category_comments$`sum(as.numeric(comment_count))`,decreasing = TRUE),]
category_comments<-category_comments[1:5,]
ggplot(category_comments,aes(x=reorder(category_name,-category_comments$`sum(as.numeric(comment_count))`),y=category_comments$`sum(as.numeric(comment_count))`))+ geom_bar(stat = "identity",fill = brewer.pal(11, "RdBu")[2])+labs(title="Japan")+
  labs(x="Category", y="Comments")+theme_bw()+theme(panel.grid=element_blank(),panel.border=element_blank(),axis.line=element_line(size=1,colour="black"))+theme(plot.title = element_text(hjust = 0.5))+theme(axis.text.x=element_text(size=10))+theme(axis.text.y = element_blank())+theme(axis.ticks.y = element_blank())
ggsave("jp_trending_comments_bar_all.png",height = 10,width = 10)


##Korea
category_comments<-summarise(group_by(krs,category_name),sum(as.numeric(comment_count)))
category_comments<-category_comments[order(category_comments$`sum(as.numeric(comment_count))`,decreasing = TRUE),]
category_comments<-category_comments[1:5,]
ggplot(category_comments,aes(x=reorder(category_name,-category_comments$`sum(as.numeric(comment_count))`),y=category_comments$`sum(as.numeric(comment_count))`))+ geom_bar(stat = "identity",fill = brewer.pal(11, "RdBu")[2])+labs(title="Korea")+
  labs(x="Category", y="Comments")+theme_bw()+theme(panel.grid=element_blank(),panel.border=element_blank(),axis.line=element_line(size=1,colour="black"))+theme(plot.title = element_text(hjust = 0.5))+theme(axis.text.x=element_text(size=10))+theme(axis.text.y = element_blank())+theme(axis.ticks.y = element_blank())
ggsave("kr_trending_comments_bar_all.png",height = 10,width = 10)

##Mexico
category_comments<-summarise(group_by(mxs,category_name),sum(as.numeric(comment_count)))
category_comments<-category_comments[order(category_comments$`sum(as.numeric(comment_count))`,decreasing = TRUE),]
category_comments<-category_comments[1:5,]
ggplot(category_comments,aes(x=reorder(category_name,-category_comments$`sum(as.numeric(comment_count))`),y=category_comments$`sum(as.numeric(comment_count))`))+ geom_bar(stat = "identity",fill = brewer.pal(11, "RdBu")[2])+labs(title="Mexico")+
  labs(x="Category", y="Comments")+theme_bw()+theme(panel.grid=element_blank(),panel.border=element_blank(),axis.line=element_line(size=1,colour="black"))+theme(plot.title = element_text(hjust = 0.5))+theme(axis.text.x=element_text(size=10))+theme(axis.text.y = element_blank())+theme(axis.ticks.y = element_blank())
ggsave("mx_trending_comments_bar_all.png",height = 10,width = 10)

##Russia
category_comments<-summarise(group_by(rus,category_name),sum(as.numeric(comment_count)))
category_comments<-category_comments[order(category_comments$`sum(as.numeric(comment_count))`,decreasing = TRUE),]
category_comments<-category_comments[1:5,]
ggplot(category_comments,aes(x=reorder(category_name,-category_comments$`sum(as.numeric(comment_count))`),y=category_comments$`sum(as.numeric(comment_count))`))+ geom_bar(stat = "identity",fill = brewer.pal(11, "RdBu")[2])+labs(title="Russia")+
  labs(x="Category", y="Comments")+theme_bw()+theme(panel.grid=element_blank(),panel.border=element_blank(),axis.line=element_line(size=1,colour="black"))+theme(plot.title = element_text(hjust = 0.5))+theme(axis.text.x=element_text(size=10))+theme(axis.text.y = element_blank())+theme(axis.ticks.y = element_blank())
ggsave("ru_trending_comments_bar_all.png",height = 10,width = 10)

#Trending times Top 5 by region
##us
category_count<-uss%>%count(category_name)
category_count<-category_count[order(category_count$n,decreasing = TRUE),]
category_count_top5<-category_count[c(1:5),]
ggplot(category_count_top5,aes(x=reorder(category_name,-n),y=n))+ geom_bar(stat = "identity",fill = brewer.pal(11, "RdBu")[2])+labs(title="United States")+
  labs(x="Category", y="Trending Times")+theme_bw()+theme(panel.grid=element_blank(),panel.border=element_blank(),axis.line=element_line(size=1,colour="black"))+theme(plot.title = element_text(hjust = 0.5))+theme(axis.text.x=element_text(size=10,angle = 90))+theme(axis.text.y = element_blank())+theme(axis.ticks.y = element_blank()) 
ggsave("us_trending_times_bar_all.png",height = 10,width = 10)


##canada
category_count<-cas%>%count(category_name)
category_count<-category_count[order(category_count$n,decreasing = TRUE),]
category_count_top5<-category_count[c(1:5),]
ggplot(category_count_top5,aes(x=reorder(category_name,-n),y=n))+ geom_bar(stat = "identity",fill = brewer.pal(11, "RdBu")[2])+labs(title="Canada")+
  labs(x="Category", y="Trending Times")+theme_bw()+theme(panel.grid=element_blank(),panel.border=element_blank(),axis.line=element_line(size=1,colour="black"))+theme(plot.title = element_text(hjust = 0.5))+theme(axis.text.x=element_text(size=10))+theme(axis.text.y = element_blank())+theme(axis.ticks.y = element_blank()) 
ggsave("ca_trending_times_bar_all.png",height = 10,width = 10)

##Germany
category_count<-des%>%count(category_name)
category_count<-category_count[order(category_count$n,decreasing = TRUE),]
category_count_top5<-category_count[c(1:5),]
ggplot(category_count_top5,aes(x=reorder(category_name,-n),y=n))+ geom_bar(stat = "identity",fill = brewer.pal(11, "RdBu")[2])+labs(title="Germany")+
  labs(x="Category", y="Trending Times")+theme_bw()+theme(panel.grid=element_blank(),panel.border=element_blank(),axis.line=element_line(size=1,colour="black"))+theme(plot.title = element_text(hjust = 0.5))+theme(axis.text.x=element_text(size=10))+theme(axis.text.y = element_blank())+theme(axis.ticks.y = element_blank()) 
ggsave("de_trending_times_bar_all.png",height = 10,width = 10)


##France
category_count<-frs%>%count(category_name)
category_count<-category_count[order(category_count$n,decreasing = TRUE),]
category_count_top5<-category_count[c(1:5),]
ggplot(category_count_top5,aes(x=reorder(category_name,-n),y=n))+ geom_bar(stat = "identity",fill = brewer.pal(11, "RdBu")[2])+labs(title="France")+
  labs(x="Category", y="Trending Times")+theme_bw()+theme(panel.grid=element_blank(),panel.border=element_blank(),axis.line=element_line(size=1,colour="black"))+theme(plot.title = element_text(hjust = 0.5))+theme(axis.text.x=element_text(size=10))+theme(axis.text.y = element_blank())+theme(axis.ticks.y = element_blank()) 
ggsave("fr_trending_times_bar_all.png",height = 10,width = 10)


##Uk
category_count<-gbs%>%count(category_name)
category_count<-category_count[order(category_count$n,decreasing = TRUE),]
category_count_top5<-category_count[c(1:5),]
ggplot(category_count_top5,aes(x=reorder(category_name,-n),y=n))+ geom_bar(stat = "identity",fill = brewer.pal(11, "RdBu")[2])+labs(title="United Kindom")+
  labs(x="Category", y="Trending Times")+theme_bw()+theme(panel.grid=element_blank(),panel.border=element_blank(),axis.line=element_line(size=1,colour="black"))+theme(plot.title = element_text(hjust = 0.5))+theme(axis.text.x=element_text(size=10))+theme(axis.text.y = element_blank())+theme(axis.ticks.y = element_blank()) 
ggsave("gb_trending_times_bar_all.png",height = 10,width = 10)


##India
category_count<-inds%>%count(category_name)
category_count<-category_count[order(category_count$n,decreasing = TRUE),]
category_count_top5<-category_count[c(1:5),]
ggplot(category_count_top5,aes(x=reorder(category_name,-n),y=n))+ geom_bar(stat = "identity",fill = brewer.pal(11, "RdBu")[2])+labs(title="India")+
  labs(x="Category", y="Trending Times")+theme_bw()+theme(panel.grid=element_blank(),panel.border=element_blank(),axis.line=element_line(size=1,colour="black"))+theme(plot.title = element_text(hjust = 0.5))+theme(axis.text.x=element_text(size=10))+theme(axis.text.y = element_blank())+theme(axis.ticks.y = element_blank()) 
ggsave("ind_trending_times_bar_all.png",height = 10,width = 10)


##Japan
category_count<-jps%>%count(category_name)
category_count<-category_count[order(category_count$n,decreasing = TRUE),]
category_count_top5<-category_count[c(1:5),]
ggplot(category_count_top5,aes(x=reorder(category_name,-n),y=n))+ geom_bar(stat = "identity",fill = brewer.pal(11, "RdBu")[2])+labs(title="Japan")+
  labs(x="Category", y="Trending Times")+theme_bw()+theme(panel.grid=element_blank(),panel.border=element_blank(),axis.line=element_line(size=1,colour="black"))+theme(plot.title = element_text(hjust = 0.5))+theme(axis.text.x=element_text(size=10))+theme(axis.text.y = element_blank())+theme(axis.ticks.y = element_blank()) 
ggsave("jp_trending_times_bar_all.png",height = 10,width = 10)


##Korea
category_count<-krs%>%count(category_name)
category_count<-category_count[order(category_count$n,decreasing = TRUE),]
category_count_top5<-category_count[c(1:5),]
ggplot(category_count_top5,aes(x=reorder(category_name,-n),y=n))+ geom_bar(stat = "identity",fill = brewer.pal(11, "RdBu")[2])+labs(title="Korea")+
  labs(x="Category", y="Trending Times")+theme_bw()+theme(panel.grid=element_blank(),panel.border=element_blank(),axis.line=element_line(size=1,colour="black"))+theme(plot.title = element_text(hjust = 0.5))+theme(axis.text.x=element_text(size=10))+theme(axis.text.y = element_blank())+theme(axis.ticks.y = element_blank()) 
ggsave("kr_trending_times_bar_all.png",height = 10,width = 10)

##Mexico
category_count<-mxs%>%count(category_name)
category_count<-category_count[order(category_count$n,decreasing = TRUE),]
category_count_top5<-category_count[c(1:5),]
ggplot(category_count_top5,aes(x=reorder(category_name,-n),y=n))+ geom_bar(stat = "identity",fill = brewer.pal(11, "RdBu")[2])+labs(title="Mexico")+
  labs(x="Category", y="Trending Times")+theme_bw()+theme(panel.grid=element_blank(),panel.border=element_blank(),axis.line=element_line(size=1,colour="black"))+theme(plot.title = element_text(hjust = 0.5))+theme(axis.text.x=element_text(size=10))+theme(axis.text.y = element_blank())+theme(axis.ticks.y = element_blank()) 
ggsave("mx_trending_times_bar_all.png",height = 10,width = 10)


##Russia
category_count<-rus%>%count(category_name)
category_count<-category_count[order(category_count$n,decreasing = TRUE),]
category_count_top5<-category_count[c(1:5),]
ggplot(category_count_top5,aes(x=reorder(category_name,-n),y=n))+ geom_bar(stat = "identity",fill = brewer.pal(11, "RdBu")[2])+labs(title="Russia")+
  labs(x="Category", y="Trending Times")+theme_bw()+theme(panel.grid=element_blank(),panel.border=element_blank(),axis.line=element_line(size=1,colour="black"))+theme(plot.title = element_text(hjust = 0.5))+theme(axis.text.x=element_text(size=20))+theme(axis.text.y = element_blank())+theme(axis.ticks.y = element_blank()) 
ggsave("ru_trending_times_bar_all.png",height = 10,width = 10)

##User participation
#global
#Calculate comment/views and (likes+dislikes)/views 
total<-mutate(total,cv=as.numeric(comment_count)/as.numeric(views),av=(as.numeric(likes)+as.numeric(dislikes))/as.numeric(views))

total<-mutate(total,trending_year=substr(trending_date,1,2),trending_month=substr(trending_date,7,8))
total_month<-total%>%group_by(trending_year,trending_month)%>%summarise(comments_month=sum(as.numeric(comment_count),na.rm = TRUE),attitude_month=sum(as.numeric(likes)+as.numeric(dislikes),na.rm = TRUE),views_month=sum(as.numeric(views),na.rm = TRUE))
total_month<-mutate(total_month,cv=as.numeric(comments_month)/as.numeric(views_month),av=(as.numeric(attitude_month))/as.numeric(views_month))


#by region
total_month_region<-total%>%group_by(country,trending_year,trending_month)%>%summarise(comments_month=sum(as.numeric(comment_count),na.rm = TRUE),attitude_month=sum(as.numeric(likes)+as.numeric(dislikes),na.rm = TRUE),views_month=sum(as.numeric(views),na.rm = TRUE))
total_month_region<-mutate(total_month_region,cv=as.numeric(comments_month)/as.numeric(views_month),av=(as.numeric(attitude_month))/as.numeric(views_month))

total_month_region<-left_join(total_month_region,total_month,by=c("trending_year" , "trending_month"))

total_month_region<-mutate(total_month_region,date=paste(trending_year,trending_month,sep="-"))
total_month_region<-mutate(total_month_region,date=paste("20",date,sep=""))
colnames(total_month_region)[8]<-"av_region"
colnames(total_month_region)[7]<-"cv_region"
colnames(total_month_region)[12]<-"cv_global"
colnames(total_month_region)[13]<-"av_global"

ggplot(total_month_region)+geom_point(mapping=aes(x=as.character(date),y=av_region,color=country))+guides(color=guide_legend(title=NULL))+theme(legend.text = element_text(color="azure4", size = 8, face = "bold"))+theme(axis.text.x=element_text(size = 6))+geom_line(mapping = aes(x=as.character(date),y=av_global,group=1),size=2,colour=brewer.pal(11, "RdBu")[2])+theme(panel.background = element_rect(fill = "transparent",colour = NA),panel.border=element_blank(),axis.line=element_line(size=1,colour="black"))+theme(plot.title = element_text(hjust = 0.5))+theme(axis.text.x=element_text(size=10))+labs(title="Average Voting Rate")+
  labs(x="Time", y="Average Voting Rate") +scale_color_discrete(labels = c('United States','Canada','Germany','France','United Kingdom','India','Japan','Korea','Mexico','Russia'))+annotate("text", x=8, y=0.028, label="Overall Voting Rate",colour=brewer.pal(11, "RdBu")[2])
ggsave("av.png",height = 10,width = 10)


ggplot(total_month_region)+geom_point(mapping=aes(x=as.character(date),y=cv_region,color=country))+guides(color=guide_legend(title=NULL))+theme(legend.text = element_text(color="azure4", size = 8, face = "bold"))+theme(axis.text.x=element_text(size = 6))+geom_line(mapping = aes(x=as.character(date),y=cv_global,group=1),size=2,colour=brewer.pal(11, "RdBu")[2])+theme(panel.background = element_rect(fill = "transparent",colour = NA),panel.border=element_blank(),axis.line=element_line(size=1,colour="black"))+theme(plot.title = element_text(hjust = 0.5))+theme(axis.text.x=element_text(size=10))+labs(title="Average Comment Rate")+
  labs(x="Time", y="Average Comment Rate") +scale_color_discrete(labels = c('United States','Canada','Germany','France','United Kingdom','India','Japan','Korea','Mexico','Russia'))+annotate("text", x=7, y=0.0025, label="Overall Comment Rate",colour=brewer.pal(11, "RdBu")[2])
ggsave("cv.png",height = 10,width = 10)

#Trending Lifecycle
##publish to initial trending
#us
usd<-mutate(usd,pub<-substr(publish_time,1,10))
colnames(usd)[12]<-"pub"
usd$pub<-as.Date(usd$pub)
usd<-mutate(usd,gov=round(-as.numeric(difftime(pub,trending_date_final,units="days")),digits = 0))
q1<-as.vector(summary(usd$gov)[2])
q3<-as.vector(summary(usd$gov)[5])
shangjie<-q3+1.5*(q3-q1)

usdgov_prep<-usd[-as.vector(which(usd$gov>shangjie)),]
usgov<-usdgov_prep%>%group_by(category_id)%>%summarise(govtime=round(mean(gov),digits = 0))
usgov<-left_join(usgov,uscategoryname,by="category_id")

ggplot(usgov,aes(x=reorder(category_name,-govtime),y=govtime,group=1))+ geom_line(size=2,colour=brewer.pal(11, "RdBu")[2])+geom_point()+labs(title="United States")+
  labs(x="Category", y="Average Going-Viral Days")+theme_bw()+theme(panel.grid=element_blank(),panel.border=element_blank(),axis.line=element_line(size=1,colour="black"))+theme(plot.title = element_text(hjust = 0.5))+theme(axis.text.x=element_text(size=10,angle = 90))+theme(axis.text.y = element_blank())+theme(axis.ticks.y = element_blank())+geom_text(aes(label = govtime, vjust = -1, hjust =0.5))+theme(title=element_text(size=20))
ggsave("us_pub_trend.png",height = 10,width = 10)

usdgov_chaochang_prep<-usd[as.vector(which(usd$gov>shangjie)),]
mean(usdgov_chaochang_prep$gov)
length(which(usd$gov>shangjie))/length(usd$gov)

#canada
cad<-mutate(cad,pub<-substr(publish_time,1,10))
colnames(cad)[11]<-"pub"
cad$pub<-as.Date(cad$pub)
cad<-mutate(cad,gov=round(-as.numeric(difftime(pub,trending_date_final,units="days")),digits = 0))
q1<-as.vector(summary(cad$gov)[2])
q3<-as.vector(summary(cad$gov)[5])
shangjie<-q3+1.5*(q3-q1)

cadgov_prep<-cad[-as.vector(which(cad$gov>shangjie)),]
cagov<-cadgov_prep%>%group_by(category_id)%>%summarise(govtime=round(mean(gov),digits = 0))
cagov<-left_join(cagov,cacategoryname,by="category_id")

ggplot(cagov,aes(x=reorder(category_name,-govtime),y=govtime,group=1))+ geom_line(size=2,colour=brewer.pal(11, "RdBu")[2])+geom_point()+labs(title="United States")+
  labs(x="Category", y="Average Going-Viral Days")+theme_bw()+theme(panel.grid=element_blank(),panel.border=element_blank(),axis.line=element_line(size=1,colour="black"))+theme(plot.title = element_text(hjust = 0.5))+theme(axis.text.x=element_text(size=10,angle = 90))+theme(axis.text.y = element_blank())+theme(axis.ticks.y = element_blank())+geom_text(aes(label = govtime, vjust = -1, hjust =0.5))+theme(title=element_text(size=20))
ggsave("ca_pub_trend.png",height = 10,width = 10)

cadgov_chaochang_prep<-cad[as.vector(which(cad$gov>shangjie)),]
mean(cadgov_chaochang_prep$gov)
length(which(cad$gov>shangjie))/length(cad$gov)

#Germany
ded<-mutate(ded,pub<-substr(publish_time,1,10))
colnames(ded)[11]<-"pub"
ded$pub<-as.Date(ded$pub)
ded<-mutate(ded,gov=round(-as.numeric(difftime(pub,trending_date_final,units="days")),digits = 0))
q1<-as.vector(summary(ded$gov)[2])
q3<-as.vector(summary(ded$gov)[5])
shangjie<-q3+1.5*(q3-q1)

dedgov_prep<-ded[-as.vector(which(ded$gov>shangjie)),]
degov<-dedgov_prep%>%group_by(category_id)%>%summarise(govtime=round(mean(gov),digits = 0))
degov<-left_join(degov,cacategoryname,by="category_id")

ggplot(degov,aes(x=reorder(category_name,-govtime),y=govtime,group=1))+ geom_line(size=2,colour=brewer.pal(11, "RdBu")[2])+geom_point()+labs(title="Germany")+
  labs(x="Category", y="Average Going-Viral Days")+theme_bw()+theme(panel.grid=element_blank(),panel.border=element_blank(),axis.line=element_line(size=1,colour="black"))+theme(plot.title = element_text(hjust = 0.5))+theme(axis.text.x=element_text(size=10,angle = 90))+theme(axis.text.y = element_blank())+theme(axis.ticks.y = element_blank())+geom_text(aes(label = govtime, vjust = -1, hjust =0.5))+theme(title=element_text(size=20))
ggsave("de_pub_trend.png",height = 10,width = 10)

dedgov_chaochang_prep<-ded[as.vector(which(ded$gov>shangjie)),]
mean(dedgov_chaochang_prep$gov)
length(which(ded$gov>shangjie))/length(ded$gov)




#France
frd<-mutate(frd,pub<-substr(publish_time,1,10))
colnames(frd)[11]<-"pub"
frd$pub<-as.Date(frd$pub)
frd<-mutate(frd,gov=round(-as.numeric(difftime(pub,trending_date_final,units="days")),digits = 0))
q1<-as.vector(summary(frd$gov)[2])
q3<-as.vector(summary(frd$gov)[5])
shangjie<-q3+1.5*(q3-q1)

frdgov_prep<-frd[-as.vector(which(frd$gov>shangjie)),]
frgov<-frdgov_prep%>%group_by(category_id)%>%summarise(govtime=round(mean(gov),digits = 0))
frgov<-left_join(frgov,cacategoryname,by="category_id")
frgov<-frgov[!is.na(frgov$govtime),]
frgov<-frgov[!is.na(frgov$category_name),]

ggplot(frgov,aes(x=reorder(category_name,-govtime),y=govtime,group=1))+ geom_line(size=2,colour=brewer.pal(11, "RdBu")[2])+geom_point()+labs(title="France")+
  labs(x="Category", y="Average Going-Viral Days")+theme_bw()+theme(panel.grid=element_blank(),panel.border=element_blank(),axis.line=element_line(size=1,colour="black"))+theme(plot.title = element_text(hjust = 0.5))+theme(axis.text.x=element_text(size=10,angle = 90))+theme(axis.text.y = element_blank())+theme(axis.ticks.y = element_blank())+geom_text(aes(label = govtime, vjust = -1, hjust =0.5))+theme(title=element_text(size=20))
ggsave("fr_pub_trend.png",height = 10,width = 10)

frdgov_chaochang_prep<-frd[as.vector(which(frd$gov>shangjie)),]
mean(frdgov_chaochang_prep$gov)
length(which(frd$gov>shangjie))/length(frd$gov)




#UK
gbd<-mutate(gbd,pub<-substr(publish_time,1,10))
colnames(gbd)[11]<-"pub"
gbd$pub<-as.Date(gbd$pub)
gbd<-mutate(gbd,gov=round(-as.numeric(difftime(pub,trending_date_final,units="days")),digits = 0))
q1<-as.vector(summary(gbd$gov)[2])
q3<-as.vector(summary(gbd$gov)[5])
shangjie<-q3+1.5*(q3-q1)

gbdgov_prep<-gbd[-as.vector(which(gbd$gov>shangjie)),]
gbgov<-gbdgov_prep%>%group_by(category_id)%>%summarise(govtime=round(mean(gov),digits = 0))
gbgov<-left_join(gbgov,cacategoryname,by="category_id")

ggplot(gbgov,aes(x=reorder(category_name,-govtime),y=govtime,group=1))+ geom_line(size=2,colour=brewer.pal(11, "RdBu")[2])+geom_point()+labs(title="United Kingdom")+
  labs(x="Category", y="Average Going-Viral Days")+theme_bw()+theme(panel.grid=element_blank(),panel.border=element_blank(),axis.line=element_line(size=1,colour="black"))+theme(plot.title = element_text(hjust = 0.5))+theme(axis.text.x=element_text(size=10,angle = 90))+theme(axis.text.y = element_blank())+theme(axis.ticks.y = element_blank())+geom_text(aes(label = govtime, vjust = -1, hjust =0.5))+theme(title=element_text(size=20))
ggsave("gb_pub_trend.png",height = 10,width = 10)

gbdgov_chaochang_prep<-gbd[as.vector(which(gbd$gov>shangjie)),]
mean(gbdgov_chaochang_prep$gov)
length(which(gbd$gov>shangjie))/length(gbd$gov)



#India
indd<-mutate(indd,pub<-substr(publish_time,1,10))
colnames(indd)[11]<-"pub"
indd$pub<-as.Date(indd$pub)
indd<-mutate(indd,gov=round(-as.numeric(difftime(pub,trending_date_final,units="days")),digits = 0))
q1<-as.vector(summary(indd$gov)[2])
q3<-as.vector(summary(indd$gov)[5])
shangjie<-q3+1.5*(q3-q1)

inddgov_prep<-indd[-as.vector(which(indd$gov>shangjie)),]
indgov<-inddgov_prep%>%group_by(category_id)%>%summarise(govtime=round(mean(gov),digits = 0))
indgov<-left_join(indgov,cacategoryname,by="category_id")
indgov<-indgov[!is.na(indgov$category_name),]

ggplot(indgov,aes(x=reorder(category_name,-govtime),y=govtime,group=1))+ geom_line(size=2,colour=brewer.pal(11, "RdBu")[2])+geom_point()+labs(title="India")+
  labs(x="Category", y="Average Going-Viral Days")+theme_bw()+theme(panel.grid=element_blank(),panel.border=element_blank(),axis.line=element_line(size=1,colour="black"))+theme(plot.title = element_text(hjust = 0.5))+theme(axis.text.x=element_text(size=10,angle = 90))+theme(axis.text.y = element_blank())+theme(axis.ticks.y = element_blank())+geom_text(aes(label = govtime, vjust = -1, hjust =0.5))+theme(title=element_text(size=20))
ggsave("ind_pub_trend.png",height = 10,width = 10)

inddgov_chaochang_prep<-indd[as.vector(which(indd$gov>shangjie)),]
mean(inddgov_chaochang_prep$gov)
length(which(indd$gov>shangjie))/length(indd$gov)


#Japan
jpd<-mutate(jpd,pub<-substr(publish_time,1,10))
colnames(jpd)[11]<-"pub"
jpd$pub<-as.Date(jpd$pub)
jpd<-mutate(jpd,gov=round(-as.numeric(difftime(pub,trending_date_final,units="days")),digits = 0))
q1<-as.vector(summary(jpd$gov)[2])
q3<-as.vector(summary(jpd$gov)[5])
shangjie<-q3+1.5*(q3-q1)

jpdgov_prep<-jpd[-as.vector(which(jpd$gov>shangjie)),]
jpgov<-jpdgov_prep%>%group_by(category_id)%>%summarise(govtime=round(mean(gov),digits = 0))
jpgov<-left_join(jpgov,cacategoryname,by="category_id")
jpgov<-jpgov[-15,]

ggplot(jpgov,aes(x=reorder(category_name,-govtime),y=govtime,group=1))+ geom_line(size=2,colour=brewer.pal(11, "RdBu")[2])+geom_point()+labs(title="Japan")+
  labs(x="Category", y="Average Going-Viral Days")+theme_bw()+theme(panel.grid=element_blank(),panel.border=element_blank(),axis.line=element_line(size=1,colour="black"))+theme(plot.title = element_text(hjust = 0.5))+theme(axis.text.x=element_text(size=10,angle = 90))+theme(axis.text.y = element_blank())+theme(axis.ticks.y = element_blank())+geom_text(aes(label = govtime, vjust = -1, hjust =0.5))+theme(title=element_text(size=20))
ggsave("jp_pub_trend.png",height = 10,width = 10)

jpdgov_chaochang_prep<-jpd[as.vector(which(jpd$gov>shangjie)),]
mean(jpdgov_chaochang_prep$gov)
length(which(jpd$gov>shangjie))/length(jpd$gov)

#Korea
krd<-mutate(krd,pub<-substr(publish_time,1,10))
colnames(krd)[11]<-"pub"
krd$pub<-as.Date(krd$pub)
krd<-mutate(krd,gov=round(-as.numeric(difftime(pub,trending_date_final,units="days")),digits = 0))
q1<-as.vector(summary(krd$gov)[2])
q3<-as.vector(summary(krd$gov)[5])
shangjie<-q3+1.5*(q3-q1)

krdgov_prep<-krd[-as.vector(which(krd$gov>shangjie)),]
krgov<-krdgov_prep%>%group_by(category_id)%>%summarise(govtime=round(mean(gov),digits = 0))
krgov<-left_join(krgov,cacategoryname,by="category_id")
krgov<-krgov[-15,]

ggplot(krgov,aes(x=reorder(category_name,-govtime),y=govtime,group=1))+ geom_line(size=2,colour=brewer.pal(11, "RdBu")[2])+geom_point()+labs(title="Korea")+
  labs(x="Category", y="Average Going-Viral Days")+theme_bw()+theme(panel.grid=element_blank(),panel.border=element_blank(),axis.line=element_line(size=1,colour="black"))+theme(plot.title = element_text(hjust = 0.5))+theme(axis.text.x=element_text(size=10,angle = 90))+theme(axis.text.y = element_blank())+theme(axis.ticks.y = element_blank())+geom_text(aes(label = govtime, vjust = -1, hjust =0.5))+theme(title=element_text(size=20))
ggsave("kr_pub_trend.png",height = 10,width = 10)

krdgov_chaochang_prep<-krd[as.vector(which(krd$gov>shangjie)),]
mean(krdgov_chaochang_prep$gov)
length(which(krd$gov>shangjie))/length(krd$gov)

#Mexico
mxd<-mutate(mxd,pub<-substr(publish_time,1,10))
colnames(mxd)[11]<-"pub"
mxd$pub<-as.Date(mxd$pub)
mxd<-mutate(mxd,gov=round(-as.numeric(difftime(pub,trending_date_final,units="days")),digits = 0))
q1<-as.vector(summary(mxd$gov)[2])
q3<-as.vector(summary(mxd$gov)[5])
shangjie<-q3+1.5*(q3-q1)

mxdgov_prep<-mxd[-as.vector(which(mxd$gov>shangjie)),]
mxgov<-mxdgov_prep%>%group_by(category_id)%>%summarise(govtime=round(mean(gov),digits = 0))
mxgov<-left_join(mxgov,cacategoryname,by="category_id")
mxgov<-mxgov[-15,]

ggplot(mxgov,aes(x=reorder(category_name,-govtime),y=govtime,group=1))+ geom_line(size=2,colour=brewer.pal(11, "RdBu")[2])+geom_point()+labs(title="Mexico")+
  labs(x="Category", y="Average Going-Viral Days")+theme_bw()+theme(panel.grid=element_blank(),panel.border=element_blank(),axis.line=element_line(size=1,colour="black"))+theme(plot.title = element_text(hjust = 0.5))+theme(axis.text.x=element_text(size=10,angle = 90))+theme(axis.text.y = element_blank())+theme(axis.ticks.y = element_blank())+geom_text(aes(label = govtime, vjust = -1, hjust =0.5))+theme(title=element_text(size=20))
ggsave("mx_pub_trend.png",height = 10,width = 10)

mxdgov_chaochang_prep<-mxd[as.vector(which(mxd$gov>shangjie)),]
mean(mxdgov_chaochang_prep$gov)
length(which(mxd$gov>shangjie))/length(mxd$gov)



#Russia
rud<-mutate(rud,pub<-substr(publish_time,1,10))
colnames(rud)[11]<-"pub"
rud$pub<-as.Date(rud$pub)
rud<-mutate(rud,gov=round(-as.numeric(difftime(pub,trending_date_final,units="days")),digits = 0))
q1<-as.vector(summary(rud$gov)[2])
q3<-as.vector(summary(rud$gov)[5])
shangjie<-q3+1.5*(q3-q1)

rudgov_prep<-rud[-as.vector(which(rud$gov>shangjie)),]
rugov<-rudgov_prep%>%group_by(category_id)%>%summarise(govtime=round(mean(gov),digits = 0))
rugov<-left_join(rugov,cacategoryname,by="category_id")
rugov<-rugov[-15,]

ggplot(rugov,aes(x=reorder(category_name,-govtime),y=govtime,group=1))+ geom_line(size=2,colour=brewer.pal(11, "RdBu")[2])+geom_point()+labs(title="Russia")+
  labs(x="Category", y="Average Going-Viral Days")+theme_bw()+theme(panel.grid=element_blank(),panel.border=element_blank(),axis.line=element_line(size=1,colour="black"))+theme(plot.title = element_text(hjust = 0.5))+theme(axis.text.x=element_text(size=10,angle = 90))+theme(axis.text.y = element_blank())+theme(axis.ticks.y = element_blank())+geom_text(aes(label = govtime, vjust = -1, hjust =0.5))+theme(title=element_text(size=20))
ggsave("ru_pub_trend.png",height = 10,width = 10)

rudgov_chaochang_prep<-rud[as.vector(which(rud$gov>shangjie)),]
mean(rudgov_chaochang_prep$gov)
length(which(rud$gov>shangjie))/length(rud$gov)

##initial trending to final trending

#us
#Calculate Trending days
usd<-us%>%mutate(p1<-paste(str_sub(as.character(us$trending_date),1,2),str_sub(as.character(us$trending_date),7,8),str_sub(as.character(us$trending_date),4,5),sep="-"))
usd<-usd%>%mutate(p2<-paste("20",usd$`... <- NULL`,sep = ""))
colnames(usd)[9]<-"trendingdate"
usd<-usd%>%mutate(as.Date(trendingdate))
colnames(usd)[10]<-"trending_date_final"

trending_lifecycle<-data.frame()
for (i in c(1:length(usd$video_id))) {
  hangshu<-which(usd$video_id==usd$video_id[i])
  yi<-as.numeric(hangshu[1])
  fuyi<-as.numeric(hangshu[length(hangshu)])
  tt<-as.numeric(difftime(usd$trending_date_final[fuyi],usd$trending_date_final[yi]))
  trending_lifecycle[i,1]<-usd$video_id[i]
  trending_lifecycle[i,2]<-tt
  
}
trending_lifecycle_dup<-trending_lifecycle[!duplicated(trending_lifecycle$V1), ]
colnames(trending_lifecycle_dup)[1]<-"video_id"
trending_lifecycle_ca<-left_join(trending_lifecycle_dup,uss,by="video_id")
trending_lifecycle_ca<-trending_lifecycle_ca[!duplicated(trending_lifecycle_ca$video_id), ]

category_count_lifecycle<-trending_lifecycle_ca%>%group_by(category_name)%>%mutate(avg_lifecycle<-mean(sum(V2)/length(video_id)))
category_count_lifecycle_ca<-category_count_lifecycle%>%select(-c(1:9))
category_count_lifecycle_ca<-category_count_lifecycle_ca[!duplicated(category_count_lifecycle_ca$`avg_lifecycle <- mean(sum(V2)/length(video_id))`),]
colnames(category_count_lifecycle_ca)[2]<-"tianshu"

ggplot(category_count_lifecycle_ca,aes(x=reorder(category_name,-tianshu),y=tianshu,group=1))+ geom_line(size=2,colour=brewer.pal(11, "RdBu")[2])+geom_point()+labs(title="United States")+
  labs(x="Category", y="Average Trending Days")+theme_bw()+theme(panel.grid=element_blank(),panel.border=element_blank(),axis.line=element_line(size=1,colour="black"))+theme(plot.title = element_text(hjust = 0.5))+theme(axis.text.x=element_text(size=10,angle = 90))+theme(axis.text.y = element_blank())+theme(axis.ticks.y = element_blank())+geom_text(aes(label = round(tianshu,digits = 2), vjust = -1, hjust =0.5))+theme(title=element_text(size=20))
ggsave("us_trending_lifecycle_bar_all.png",height = 10,width = 10)

#ca
cad<-ca%>%mutate(p1<-paste(str_sub(as.character(ca$trending_date),1,2),str_sub(as.character(ca$trending_date),7,8),str_sub(as.character(ca$trending_date),4,5),sep="-"))
cad<-cad%>%mutate(p2<-paste("20",cad$`... <- NULL`,sep = ""))
colnames(cad)[9]<-"trendingdate"
colnames(cad)[10]<-"trending_date_final"

trending_lifecycle<-data.frame()
for (i in c(1:length(cad$video_id))) {
  hangshu<-which(cad$video_id==cad$video_id[i])
  yi<-as.numeric(hangshu[1])
  fuyi<-as.numeric(hangshu[length(hangshu)])
  tt<-as.numeric(difftime(cad$trending_date_final[fuyi],cad$trending_date_final[yi]))
  trending_lifecycle[i,1]<-cad$video_id[i]
  trending_lifecycle[i,2]<-tt
}

trending_lifecycle_dup<-trending_lifecycle[!duplicated(trending_lifecycle$V1), ]
colnames(trending_lifecycle_dup)[1]<-"video_id"
trending_lifecycle_ca<-left_join(trending_lifecycle_dup,cas,by="video_id")
trending_lifecycle_ca<-trending_lifecycle_ca[!duplicated(trending_lifecycle_ca$video_id), ]

category_count_lifecycle<-trending_lifecycle_ca%>%group_by(category_name)%>%mutate(avg_lifecycle<-mean(sum(V2)/length(video_id)))
category_count_lifecycle_ca<-category_count_lifecycle%>%select(-c(1:9))
category_count_lifecycle_ca<-category_count_lifecycle_ca[!duplicated(category_count_lifecycle_ca$`avg_lifecycle <- mean(sum(V2)/length(video_id))`),]
colnames(category_count_lifecycle_ca)[2]<-"tianshu"
category_count_lifecycle_ca<-category_count_lifecycle_ca[-11,]

ggplot(category_count_lifecycle_ca,aes(x=reorder(category_name,-tianshu),y=tianshu,group=1))+ geom_line(size=2,colour=brewer.pal(11, "RdBu")[2])+geom_point()+labs(title="Canada")+
  labs(x="Category", y="Average Trending Days")+theme_bw()+theme(panel.grid=element_blank(),panel.border=element_blank(),axis.line=element_line(size=1,colour="black"))+theme(plot.title = element_text(hjust = 0.5))+theme(axis.text.x=element_text(size=10,angle = 90))+theme(axis.text.y = element_blank())+theme(axis.ticks.y = element_blank())+geom_text(aes(label = round(tianshu,digits = 2), vjust = -1, hjust =0.5))+theme(title=element_text(size=20))
ggsave("ca_trending_lifecycle_bar_all.png",height = 10,width = 10)


#Germany
ded<-de%>%mutate(p1<-paste(str_sub(as.character(de$trending_date),1,2),str_sub(as.character(de$trending_date),7,8),str_sub(as.character(de$trending_date),4,5),sep="-"))
ded<-ded%>%mutate(p2<-paste("20",ded$`... <- NULL`,sep = ""))
ded<-ded[,-c(9:34)]
colnames(ded)[9]<-"trendingdate"
colnames(ded)[10]<-"trending_date_final"

trending_lifecycle<-data.frame()
for (i in c(1:length(ded$video_id))) {
  hangshu<-which(ded$video_id==ded$video_id[i])
  yi<-as.numeric(hangshu[1])
  fuyi<-as.numeric(hangshu[length(hangshu)])
  tt<-as.numeric(difftime(ded$trending_date_final[fuyi],ded$trending_date_final[yi]))
  trending_lifecycle[i,1]<-ded$video_id[i]
  trending_lifecycle[i,2]<-tt
}

trending_lifecycle_dup<-trending_lifecycle[!duplicated(trending_lifecycle$V1), ]
colnames(trending_lifecycle_dup)[1]<-"video_id"
trending_lifecycle_ca<-left_join(trending_lifecycle_dup,des,by="video_id")
trending_lifecycle_ca<-trending_lifecycle_ca[!duplicated(trending_lifecycle_ca$video_id), ]

category_count_lifecycle<-trending_lifecycle_ca%>%group_by(category_name)%>%mutate(avg_lifecycle<-mean(sum(V2)/length(video_id)))
category_count_lifecycle_ca<-category_count_lifecycle%>%select(-c(1:9))
category_count_lifecycle_ca<-category_count_lifecycle_ca[!duplicated(category_count_lifecycle_ca$`avg_lifecycle <- mean(sum(V2)/length(video_id))`),]
colnames(category_count_lifecycle_ca)[2]<-"tianshu"
category_count_lifecycle_ca<-category_count_lifecycle_ca[-14,]

ggplot(category_count_lifecycle_ca,aes(x=reorder(category_name,-tianshu),y=tianshu,group=1))+ geom_line(size=2,colour=brewer.pal(11, "RdBu")[2])+geom_point()+labs(title="Germany")+
  labs(x="Category", y="Average Trending Days")+theme_bw()+theme(panel.grid=element_blank(),panel.border=element_blank(),axis.line=element_line(size=1,colour="black"))+theme(plot.title = element_text(hjust = 0.5))+theme(axis.text.x=element_text(size=10,angle = 90))+theme(axis.text.y = element_blank())+theme(axis.ticks.y = element_blank())+geom_text(aes(label = round(tianshu,digits = 2), vjust = -1, hjust =0.5))+theme(title=element_text(size=20))
ggsave("de_trending_lifecycle_bar_all.png",height = 10,width = 10)

#France
frd<-fr%>%mutate(p1<-paste(str_sub(as.character(fr$trending_date),1,2),str_sub(as.character(fr$trending_date),7,8),str_sub(as.character(fr$trending_date),4,5),sep="-"))
frd<-frd%>%mutate(p2<-paste("20",frd$`... <- NULL`,sep = ""))
frd<-frd[,-c(9:47)]
colnames(frd)[9]<-"trendingdate"
colnames(frd)[10]<-"trending_date_final"

trending_lifecycle<-data.frame()
for (i in c(1:length(frd$video_id))) {
  hangshu<-which(frd$video_id==frd$video_id[i])
  yi<-as.numeric(hangshu[1])
  fuyi<-as.numeric(hangshu[length(hangshu)])
  tt<-as.numeric(difftime(frd$trending_date_final[fuyi],frd$trending_date_final[yi]))
  trending_lifecycle[i,1]<-frd$video_id[i]
  trending_lifecycle[i,2]<-tt
}

trending_lifecycle_dup<-trending_lifecycle[!duplicated(trending_lifecycle$V1), ]
colnames(trending_lifecycle_dup)[1]<-"video_id"
trending_lifecycle_ca<-left_join(trending_lifecycle_dup,frs,by="video_id")
trending_lifecycle_ca<-trending_lifecycle_ca[!duplicated(trending_lifecycle_ca$video_id), ]

category_count_lifecycle<-trending_lifecycle_ca%>%group_by(category_name)%>%mutate(avg_lifecycle<-mean(sum(V2)/length(video_id)))
category_count_lifecycle_ca<-category_count_lifecycle%>%select(-c(1:9))
category_count_lifecycle_ca<-category_count_lifecycle_ca[!duplicated(category_count_lifecycle_ca$`avg_lifecycle <- mean(sum(V2)/length(video_id))`),]
colnames(category_count_lifecycle_ca)[2]<-"tianshu"
category_count_lifecycle_ca<-category_count_lifecycle_ca[-1,]

ggplot(category_count_lifecycle_ca,aes(x=reorder(category_name,-tianshu),y=tianshu,group=1))+ geom_line(size=2,colour=brewer.pal(11, "RdBu")[2])+geom_point()+labs(title="France")+
  labs(x="Category", y="Average Trending Days")+theme_bw()+theme(panel.grid=element_blank(),panel.border=element_blank(),axis.line=element_line(size=1,colour="black"))+theme(plot.title = element_text(hjust = 0.5))+theme(axis.text.x=element_text(size=10,angle = 90))+theme(axis.text.y = element_blank())+theme(axis.ticks.y = element_blank())+geom_text(aes(label = round(tianshu,digits = 2), vjust = -1, hjust =0.5))+theme(title=element_text(size=20))
ggsave("fr_trending_lifecycle_bar_all.png",height = 10,width = 10)

#UK
gbd<-gb%>%mutate(p1<-paste(str_sub(as.character(gb$trending_date),1,2),str_sub(as.character(gb$trending_date),7,8),str_sub(as.character(gb$trending_date),4,5),sep="-"))
gbd<-gbd%>%mutate(p2<-paste("20",gbd$`... <- NULL`,sep = ""))
gbd<-gbd[,-c(9:34)]
colnames(gbd)[9]<-"trendingdate"
colnames(gbd)[10]<-"trending_date_final"

trending_lifecycle<-data.frame()
for (i in c(1:length(gbd$video_id))) {
  hangshu<-which(gbd$video_id==gbd$video_id[i])
  yi<-as.numeric(hangshu[1])
  fuyi<-as.numeric(hangshu[length(hangshu)])
  tt<-as.numeric(difftime(gbd$trending_date_final[fuyi],gbd$trending_date_final[yi]))
  trending_lifecycle[i,1]<-gbd$video_id[i]
  trending_lifecycle[i,2]<-tt
}

trending_lifecycle_dup<-trending_lifecycle[!duplicated(trending_lifecycle$V1), ]
colnames(trending_lifecycle_dup)[1]<-"video_id"
trending_lifecycle_ca<-left_join(trending_lifecycle_dup,gbs,by="video_id")
trending_lifecycle_ca<-trending_lifecycle_ca[!duplicated(trending_lifecycle_ca$video_id), ]

category_count_lifecycle<-trending_lifecycle_ca%>%group_by(category_name)%>%mutate(avg_lifecycle<-mean(sum(V2)/length(video_id)))
category_count_lifecycle_ca<-category_count_lifecycle%>%select(-c(1:9))
category_count_lifecycle_ca<-category_count_lifecycle_ca[!duplicated(category_count_lifecycle_ca$`avg_lifecycle <- mean(sum(V2)/length(video_id))`),]
colnames(category_count_lifecycle_ca)[2]<-"tianshu"
category_count_lifecycle_ca<-category_count_lifecycle_ca[-15,]

ggplot(category_count_lifecycle_ca,aes(x=reorder(category_name,-tianshu),y=tianshu,group=1))+ geom_line(size=2,colour=brewer.pal(11, "RdBu")[2])+geom_point()+labs(title="United Kindom")+
  labs(x="Category", y="Average Trending Days")+theme_bw()+theme(panel.grid=element_blank(),panel.border=element_blank(),axis.line=element_line(size=1,colour="black"))+theme(plot.title = element_text(hjust = 0.5))+theme(axis.text.x=element_text(size=10,angle = 90))+theme(axis.text.y = element_blank())+theme(axis.ticks.y = element_blank())+geom_text(aes(label = round(tianshu,digits = 2), vjust = -1, hjust =0.5))+theme(title=element_text(size=20))
ggsave("gb_trending_lifecycle_bar_all.png",height = 10,width = 10)

#India
indd<-ind%>%mutate(p1<-paste(str_sub(as.character(ind$trending_date),1,2),str_sub(as.character(ind$trending_date),7,8),str_sub(as.character(ind$trending_date),4,5),sep="-"))
indd<-indd%>%mutate(p2<-paste("20",indd$`... <- NULL`,sep = ""))
indd<-indd[,-c(9:26)]
colnames(indd)[9]<-"trendingdate"
colnames(indd)[10]<-"trending_date_final"

trending_lifecycle<-data.frame()
for (i in c(1:length(indd$video_id))) {
  hangshu<-which(indd$video_id==indd$video_id[i])
  yi<-as.numeric(hangshu[1])
  fuyi<-as.numeric(hangshu[length(hangshu)])
  tt<-as.numeric(difftime(indd$trending_date_final[fuyi],indd$trending_date_final[yi]))
  trending_lifecycle[i,1]<-indd$video_id[i]
  trending_lifecycle[i,2]<-tt
}

trending_lifecycle_dup<-trending_lifecycle[!duplicated(trending_lifecycle$V1), ]
colnames(trending_lifecycle_dup)[1]<-"video_id"
trending_lifecycle_ca<-left_join(trending_lifecycle_dup,inds,by="video_id")
trending_lifecycle_ca<-trending_lifecycle_ca[!duplicated(trending_lifecycle_ca$video_id), ]

category_count_lifecycle<-trending_lifecycle_ca%>%group_by(category_name)%>%mutate(avg_lifecycle<-mean(sum(V2)/length(video_id)))
category_count_lifecycle_ca<-category_count_lifecycle%>%select(-c(1:9))
category_count_lifecycle_ca<-category_count_lifecycle_ca[!duplicated(category_count_lifecycle_ca$`avg_lifecycle <- mean(sum(V2)/length(video_id))`),]
colnames(category_count_lifecycle_ca)[2]<-"tianshu"
category_count_lifecycle_ca<-category_count_lifecycle_ca[-2,]

ggplot(category_count_lifecycle_ca,aes(x=reorder(category_name,-tianshu),y=tianshu,group=1))+ geom_line(size=2,colour=brewer.pal(11, "RdBu")[2])+geom_point()+labs(title="India")+
  labs(x="Category", y="Average Trending Days")+theme_bw()+theme(panel.grid=element_blank(),panel.border=element_blank(),axis.line=element_line(size=1,colour="black"))+theme(plot.title = element_text(hjust = 0.5))+theme(axis.text.x=element_text(size=10,angle = 90))+theme(axis.text.y = element_blank())+theme(axis.ticks.y = element_blank())+geom_text(aes(label = round(tianshu,digits = 2), vjust = -1, hjust =0.5))+theme(title=element_text(size=20))
ggsave("ind_trending_lifecycle_bar_all.png",height = 10,width = 10)

#Japan
jpd<-jp%>%mutate(p1<-paste(str_sub(as.character(jp$trending_date),1,2),str_sub(as.character(jp$trending_date),7,8),str_sub(as.character(jp$trending_date),4,5),sep="-"))
jpd<-jpd%>%mutate(p2<-paste("20",jpd$`... <- NULL`,sep = ""))
colnames(jpd)[9]<-"trendingdate"
colnames(jpd)[10]<-"trending_date_final"

trending_lifecycle<-data.frame()
for (i in c(1:length(jpd$video_id))) {
  hangshu<-which(jpd$video_id==jpd$video_id[i])
  yi<-as.numeric(hangshu[1])
  fuyi<-as.numeric(hangshu[length(hangshu)])
  tt<-as.numeric(difftime(jpd$trending_date_final[fuyi],jpd$trending_date_final[yi]))
  trending_lifecycle[i,1]<-jpd$video_id[i]
  trending_lifecycle[i,2]<-tt
}

trending_lifecycle_dup<-trending_lifecycle[!duplicated(trending_lifecycle$V1), ]
colnames(trending_lifecycle_dup)[1]<-"video_id"
trending_lifecycle_ca<-left_join(trending_lifecycle_dup,jps,by="video_id")
trending_lifecycle_ca<-trending_lifecycle_ca[!duplicated(trending_lifecycle_ca$video_id), ]

category_count_lifecycle<-trending_lifecycle_ca%>%group_by(category_name)%>%mutate(avg_lifecycle<-mean(sum(V2)/length(video_id)))
category_count_lifecycle_ca<-category_count_lifecycle%>%select(-c(1:9))
category_count_lifecycle_ca<-category_count_lifecycle_ca[!duplicated(category_count_lifecycle_ca$`avg_lifecycle <- mean(sum(V2)/length(video_id))`),]
colnames(category_count_lifecycle_ca)[2]<-"tianshu"


ggplot(category_count_lifecycle_ca,aes(x=reorder(category_name,-tianshu),y=tianshu,group=1))+ geom_line(size=2,colour=brewer.pal(11, "RdBu")[2])+geom_point()+labs(title="Japan")+
  labs(x="Category", y="Average Trending Days")+theme_bw()+theme(panel.grid=element_blank(),panel.border=element_blank(),axis.line=element_line(size=1,colour="black"))+theme(plot.title = element_text(hjust = 0.5))+theme(axis.text.x=element_text(size=10,angle = 90))+theme(axis.text.y = element_blank())+theme(axis.ticks.y = element_blank())+geom_text(aes(label = round(tianshu,digits = 2), vjust = -1, hjust =0.5))+theme(title=element_text(size=20))
ggsave("jp_trending_lifecycle_bar_all.png",height = 10,width = 10)


#Korea
krd<-kr%>%mutate(p1<-paste(str_sub(as.character(kr$trending_date),1,2),str_sub(as.character(kr$trending_date),7,8),str_sub(as.character(kr$trending_date),4,5),sep="-"))
krd<-krd%>%mutate(p2<-paste("20",krd$`... <- NULL`,sep = ""))
krd<-krd[,-c(9:41)]
colnames(krd)[9]<-"trendingdate"
colnames(krd)[10]<-"trending_date_final"

trending_lifecycle<-data.frame()
for (i in c(1:length(krd$video_id))) {
  hangshu<-which(krd$video_id==krd$video_id[i])
  yi<-as.numeric(hangshu[1])
  fuyi<-as.numeric(hangshu[length(hangshu)])
  tt<-as.numeric(difftime(krd$trending_date_final[fuyi],krd$trending_date_final[yi]))
  trending_lifecycle[i,1]<-krd$video_id[i]
  trending_lifecycle[i,2]<-tt
}

trending_lifecycle_dup<-trending_lifecycle[!duplicated(trending_lifecycle$V1), ]
colnames(trending_lifecycle_dup)[1]<-"video_id"
trending_lifecycle_ca<-left_join(trending_lifecycle_dup,krs,by="video_id")
trending_lifecycle_ca<-trending_lifecycle_ca[!duplicated(trending_lifecycle_ca$video_id), ]

category_count_lifecycle<-trending_lifecycle_ca%>%group_by(category_name)%>%mutate(avg_lifecycle<-mean(sum(V2)/length(video_id)))
category_count_lifecycle_ca<-category_count_lifecycle%>%select(-c(1:9))
category_count_lifecycle_ca<-category_count_lifecycle_ca[!duplicated(category_count_lifecycle_ca$`avg_lifecycle <- mean(sum(V2)/length(video_id))`),]
colnames(category_count_lifecycle_ca)[2]<-"tianshu"
category_count_lifecycle_ca<-category_count_lifecycle_ca[-15,]

ggplot(category_count_lifecycle_ca,aes(x=reorder(category_name,-tianshu),y=tianshu,group=1))+ geom_line(size=2,colour=brewer.pal(11, "RdBu")[2])+geom_point()+labs(title="Korea")+
  labs(x="Category", y="Average Trending Days")+theme_bw()+theme(panel.grid=element_blank(),panel.border=element_blank(),axis.line=element_line(size=1,colour="black"))+theme(plot.title = element_text(hjust = 0.5))+theme(axis.text.x=element_text(size=10,angle = 90))+theme(axis.text.y = element_blank())+theme(axis.ticks.y = element_blank())+geom_text(aes(label = round(tianshu,digits = 2), vjust = -1, hjust =0.5))+theme(title=element_text(size=20))
ggsave("kr_trending_lifecycle_bar_all.png",height = 10,width = 10)


#Mexico
mxd<-mx%>%mutate(p1<-paste(str_sub(as.character(mx$trending_date),1,2),str_sub(as.character(mx$trending_date),7,8),str_sub(as.character(mx$trending_date),4,5),sep="-"))
mxd<-mxd%>%mutate(p2<-paste("20",mxd$`... <- NULL`,sep = ""))
mxd<-mxd[,-c(9:37)]
colnames(mxd)[9]<-"trendingdate"
colnames(mxd)[10]<-"trending_date_final"

trending_lifecycle<-data.frame()
for (i in c(1:length(mxd$video_id))) {
  hangshu<-which(mxd$video_id==mxd$video_id[i])
  yi<-as.numeric(hangshu[1])
  fuyi<-as.numeric(hangshu[length(hangshu)])
  tt<-as.numeric(difftime(mxd$trending_date_final[fuyi],mxd$trending_date_final[yi]))
  trending_lifecycle[i,1]<-mxd$video_id[i]
  trending_lifecycle[i,2]<-tt
}

trending_lifecycle_dup<-trending_lifecycle[!duplicated(trending_lifecycle$V1), ]
colnames(trending_lifecycle_dup)[1]<-"video_id"
trending_lifecycle_ca<-left_join(trending_lifecycle_dup,mxs,by="video_id")
trending_lifecycle_ca<-trending_lifecycle_ca[!duplicated(trending_lifecycle_ca$video_id), ]

category_count_lifecycle<-trending_lifecycle_ca%>%group_by(category_name)%>%mutate(avg_lifecycle<-mean(sum(V2)/length(video_id)))
category_count_lifecycle_ca<-category_count_lifecycle%>%select(-c(1:9))
category_count_lifecycle_ca<-category_count_lifecycle_ca[!duplicated(category_count_lifecycle_ca$`avg_lifecycle <- mean(sum(V2)/length(video_id))`),]
colnames(category_count_lifecycle_ca)[2]<-"tianshu"
category_count_lifecycle_ca<-category_count_lifecycle_ca[-15,]

ggplot(category_count_lifecycle_ca,aes(x=reorder(category_name,-tianshu),y=tianshu,group=1))+ geom_line(size=2,colour=brewer.pal(11, "RdBu")[2])+geom_point()+labs(title="Mexico")+
  labs(x="Category", y="Average Trending Days")+theme_bw()+theme(panel.grid=element_blank(),panel.border=element_blank(),axis.line=element_line(size=1,colour="black"))+theme(plot.title = element_text(hjust = 0.5))+theme(axis.text.x=element_text(size=10,angle = 90))+theme(axis.text.y = element_blank())+theme(axis.ticks.y = element_blank())+geom_text(aes(label = round(tianshu,digits = 2), vjust = -1, hjust =0.5))+theme(title=element_text(size=20))
ggsave("mx_trending_lifecycle_bar_all.png",height = 10,width = 10)






#Russia
rud<-ru%>%mutate(p1<-paste(str_sub(as.character(ru$trending_date),1,2),str_sub(as.character(ru$trending_date),7,8),str_sub(as.character(ru$trending_date),4,5),sep="-"))
rud<-rud%>%mutate(p2<-paste("20",rud$`... <- NULL`,sep = ""))
rud<-rud[,-c(9:37)]
colnames(rud)[9]<-"trendingdate"
colnames(rud)[10]<-"trending_date_final"

trending_lifecycle<-data.frame()
for (i in c(1:length(rud$video_id))) {
  hangshu<-which(rud$video_id==rud$video_id[i])
  yi<-as.numeric(hangshu[1])
  fuyi<-as.numeric(hangshu[length(hangshu)])
  tt<-as.numeric(difftime(rud$trending_date_final[fuyi],rud$trending_date_final[yi]))
  trending_lifecycle[i,1]<-rud$video_id[i]
  trending_lifecycle[i,2]<-tt
}

trending_lifecycle_dup<-trending_lifecycle[!duplicated(trending_lifecycle$V1), ]
colnames(trending_lifecycle_dup)[1]<-"video_id"
trending_lifecycle_ca<-left_join(trending_lifecycle_dup,rus,by="video_id")
trending_lifecycle_ca<-trending_lifecycle_ca[!duplicated(trending_lifecycle_ca$video_id), ]

category_count_lifecycle<-trending_lifecycle_ca%>%group_by(category_name)%>%mutate(avg_lifecycle<-mean(sum(V2)/length(video_id)))
category_count_lifecycle_ca<-category_count_lifecycle%>%select(-c(1:9))
category_count_lifecycle_ca<-category_count_lifecycle_ca[!duplicated(category_count_lifecycle_ca$`avg_lifecycle <- mean(sum(V2)/length(video_id))`),]
colnames(category_count_lifecycle_ca)[2]<-"tianshu"
category_count_lifecycle_ca<-category_count_lifecycle_ca[-15,]

ggplot(category_count_lifecycle_ca,aes(x=reorder(category_name,-tianshu),y=tianshu,group=1))+ geom_line(size=2,colour=brewer.pal(11, "RdBu")[2])+geom_point()+labs(title="Russia")+
  labs(x="Category", y="Average Trending Days")+theme_bw()+theme(panel.grid=element_blank(),panel.border=element_blank(),axis.line=element_line(size=1,colour="black"))+theme(plot.title = element_text(hjust = 0.5))+theme(axis.text.x=element_text(size=10,angle = 90))+theme(axis.text.y = element_blank())+theme(axis.ticks.y = element_blank())+geom_text(aes(label = round(tianshu,digits = 2), vjust = -1, hjust =0.5))+theme(title=element_text(size=20))
ggsave("ru_trending_lifecycle_bar_all.png",height = 10,width = 10)

#Modelling--Multilevel model
library(lme4)
total<-total%>%mutate(p1<-paste(str_sub(as.character(total$trending_date),1,2),str_sub(as.character(total$trending_date),7,8),str_sub(as.character(total$trending_date),4,5),sep="-"))
total<-total%>%mutate(p2<-paste("20",total$`... <- NULL`,sep = ""))
colnames(total)[11]<-"trendingdate"
total<-total%>%mutate(as.Date(trendingdate))
colnames(total)[12]<-"trending_date_final"



total<-mutate(total,pub<-substr(publish_time,1,10))
colnames(total)[18]<-"pub"
total$pub<-as.Date(total$pub)
total<-mutate(total,gov=round(-as.numeric(difftime(pub,trending_date_final,units="days")),digits = 0))
totall<-total[!duplicated(total$video_id),]
totalll<-totall[which(totall$trending_year==18),]
totallll<-totalll[which(totalll$trending_month=="06"),]
totallll$views<-as.numeric(totallll$views)
totallll<-mutate(totallll,view_ct=(views-mean(views,na.rm=TRUE))/sd(views,na.rm = TRUE))

model1 <- lmer(gov~1+(1+av+cv+view_ct|country),data=totallll)
model2 <- lmer(gov~1+av+cv+view_ct+(1|country),data=totallll)
model3 <- lmer(gov~av+cv+view_ct+(1+av+cv+view_ct|country),data=totallll)










