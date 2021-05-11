library("dplyr")
library("ggpubr")
library("car")
##############################################################
youtube <- read.csv("youtube.csv")
test <- read.csv("youtube_test.csv")
#youtube
youtube <- youtube %>% rename(views = X.ปฟviews,)
test <- test %>% rename(views = X.ปฟviews,)
summary(youtube)
youtube %>% summarise( count = n(), mean = mean(views,na.rm = TRUE), sd = sd(views, na.rm = TRUE) )
youtube %>% summarise( count = n(), mean = mean(likes,na.rm = TRUE), sd = sd(likes, na.rm = TRUE) )
youtube %>% summarise( count = n(), mean = mean(dislikes,na.rm = TRUE), sd = sd(dislikes, na.rm = TRUE) )
###############################################################
shapiro.test(youtube$views)
shapiro.test(youtube$likes)
shapiro.test(youtube$dislikes)
shapiro.test(youtube$comment_count)
###############################################################
res_1 <- cor.test(youtube$likes, youtube$views,method = "spearman")
res_1

res_2 <- cor.test(youtube$dislikes, youtube$views,method = "spearman")
res_2

res_3 <- cor.test(youtube$comment_count, youtube$views,method = "spearman")
res_3

res_4 <- cor.test(youtube$likes, youtube$dislikes,method = "spearman")
res_4

res_5 <- cor.test(youtube$comment_count, youtube$dislikes,method = "spearman")
res_5

res_6 <- cor.test(youtube$comment_count, youtube$likes,method = "spearman")
res_6
###############################################################
col<- colorRampPalette(c("blue", "white", "red"))(20)
cormat<- rquery.cormat(youtube, col=col)

###############################################################
ggplot(youtube, aes(x = likes, y = views)) + geom_point() + stat_smooth()
ggplot(youtube, aes(x = dislikes, y = views)) + geom_point() + stat_smooth()
ggplot(youtube, aes(x = comment_count, y = views)) + geom_point() + stat_smooth()
ggplot(youtube, aes(x = likes, y = dislikes)) + geom_point() + stat_smooth()
ggplot(youtube, aes(x = comment_count, y = dislikes)) + geom_point() + stat_smooth()
ggplot(youtube, aes(x = comment_count, y = likes)) + geom_point() + stat_smooth()
###############################################################
ggscatter(youtube, x = "likes", y = "views", add ="reg.line", conf.int = TRUE, cor.coef = TRUE,cor.method = "spearman", xlab = "Likes", ylab = "Views")
ggscatter(youtube, x = "dislikes", y = "views", add ="reg.line", conf.int = TRUE, cor.coef = TRUE,cor.method = "spearman", xlab = "Dislikes", ylab = "Views")
ggscatter(youtube, x = "comment_count", y = "views", add ="reg.line", conf.int = TRUE, cor.coef = TRUE,cor.method = "spearman", xlab = "Comment_count", ylab = "Views")
ggscatter(youtube, x = "likes", y = "dislikes", add ="reg.line", conf.int = TRUE, cor.coef = TRUE,cor.method = "spearman", xlab = "Likes", ylab = "Dislikes")
ggscatter(youtube, x = "comment_count", y = "dislikes", add ="reg.line", conf.int = TRUE, cor.coef = TRUE,cor.method = "spearman", xlab = "Comment_count", ylab = "Dislikes")
ggscatter(youtube, x = "comment_count", y = "likes", add ="reg.line", conf.int = TRUE, cor.coef = TRUE,cor.method = "spearman", xlab = "Comment_count", ylab = "Likes")

##############################################################
model_3 <- lm(views ~ likes + dislikes + comment_count, data = youtube) 
summary(model_3)

model_3 <- lm(views ~ likes + dislikes, data = youtube)
summary(model_3)
confint(model_3,level = 0.95)
Anova(model_3)
sigma(model)*100/mean(youtube$views)
##############################################################
yhat<-predict(model_3,test)
yhat
test
plot(test$views,yhat)
##############################################################
