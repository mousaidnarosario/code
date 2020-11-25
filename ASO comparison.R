###
#title: ASO exploratory data comparison
#       #The data is a draft of testing different ASO techniques to understand which strategy is better to improve a 
#       game's visibility in Google and iOS.  ANOVA, Tukey,
        #and difference in means are deployed in the code after comparing the observed data to expected data of 100. 
#author: "Mousaidna Rosario"
###



reg <- c(3,4,5,3,2,4,3,3,5,4)
intern <- c(4,3,4,2,4,3,4,5,4,3)

employees <- matrix(cbind(reg, intern), ncol =2
       )

colnames(employees) <- c('Regular',"Interns" )

employees<- as.data.frame(employees)

barplot(colSums(employees),main='Summary of Employment of Each Gaming companies', col=c("red", "blue"))



Marketing.app <- c('Yes','No','No','Yes','Yes','Yes','Yes','Yes')



d <- replicate(10,
               {
                 c('Yes','No','No','Yes','Yes','Yes','Yes','Yes',"Yes",'Yes' )
               })

barplot(table(d), main= "Respondents involves you in marketing", col=c("red", "blue"), ylab="Respondents"
        ,xlab="No. of respondents")


dt <- table(d)

dt/sum(dt)


ASO.user <- c('Yes','No','No','Yes','Yes','Yes','Yes','Yes','Yes','Yes')

at <- table(ASO.user)

a.user <- sample(c("No","Yes"), 100, replace=TRUE,prob = at/sum(at))

barplot(table(a.user), main="Respondents involves you in marketing", ylab="Respondents"
        ,xlab="Answers"
        ,col= c("red","blue"))


######### What makes your game unique among other apps?

#colors
install.packages("RColorBrewer")
library("RColorBrewer")

install.packages("wesanderson")
library(?wesanderson)
unique <- c("Design","Design","Others","Level of Difficulty","Level of Difficulty","Level of Difficulty","Level of Difficulty"
            ,"Community and user interaction","Fun","Fun")

u1 <- replicate(10,
                {
                  unique
                })

x <-c("Eye catching graphics", "Level of Difficulty","Community","Fun","Design and responsiveness","Others")

table(u1)

colnames(u1) <- c("Graphics", "Difficulty","User Interaction","Fun","Design","Others")
rownames(u1) <- "Respondents"



du1 <- table(u1)

u/sum(u)*100
u <- matrix(rowSums(u1), ncol=6)
cols = c("blue", "red","orange","green","purple","light blue")
pal <- colorRampPalette(colors = c("lightblue", "blue"))(6)

barplot(du1, col=heat.colors(6), main ="What makes your Game app unique", xlab="Uniqueness",ylab="Respondents")

display.brewer.all() 

barplot(1:5, col=heat.colors(5))

barplot(du1 , main="What make your app unique",  col = pal)

legend.text = c("raphics", "Difficulty","User Interaction","Fun","Design","Others")
args.legend = list('topright')


#challenges as a company

na <- c(5,5,5,5,4,3,3,2)

table(na)

na.dup <- replicate(10,
               {
                 na
               })

table(na.dup)

no.attn <- matrix(rowSums(na.dup), ncol=5)

barplot(table(na.dup), main= "Challenges you face as a gaming company:\n No attention in the app", 
        col=heat.colors(),xlab="Rating",ylab="Respondents")


#lack of fund


lf <- c(5,5,5,5,4,4,4,3)

lf.dup <- replicate(10,
                    {
                      lf
                    })

table(lf.dup)

barplot(table(lf.dup), main= "Challenges you face as a gaming company:\n Lack of Fund", 
        col=rainbow(5),xlab="Rating",ylab="Respondents")


#Project Management Skills



pm <- c(5,3,3,3,3,3,2,1)

pm.dup <- replicate(10,
                    {
                      pm
                    })

table(pm.dup)

barplot(table(pm.dup), main= "Challenges you face as a gaming company:\n Lack of Project Management Skills", 
       col=rainbow(5),xlab="Rating",ylab="Respondents")



#Project Management Skills


ue <- c(4,4,4,3,2,2,2,1)

ue.dup <- replicate(10,
                    {
                      ue
                    })

table(ue.dup)

barplot(table(ue.dup), main= "Challenges you face as a gaming company:\n User Engagement & Loyalty", 
        col=rainbow(5),xlab="Rating",ylab="Respondents")


###

unique <- c(0,2, 1, 3,1,1)
u1 <- replicate(10,
                {
                  unique
                })

u <- matrix(rowSums(u1), ncol=6)

colnames(u) <- c("Graphics", "Difficulty","User Interaction","Fun","Design","Others")
rownames(u) <- "Respondents"
pal <- colorRampPalette(colors = c("lightblue", "blue"))(6)


colnames(u) <- c("Graphics", "Difficulty","User Interaction","Fun","Design","Others")
rownames(u) <- "Respondents"
pal <- colorRampPalette(colors = c("lightblue", "blue"))(6)

barplot(as.vector(u) , main="What makes the your app unique",  col = heat.colors(6),
names.arg = c("Graphics", "Difficulty","User Interaction","Fun","Design","Others") )

### rating 5
rating.5 <-c("No attention in the platform","No attention in the platform",
"No attention in the platform","No attention in the platform","Lack Of fund","Lack Of fund"
,"Lack Of fund","Lack Of fund","Project Mngmnt Skills", "Not getting right human resources",
"Lack of knowledge in branding and promotion")

table(rating.5)

rating5 <- c(4,4,1,0,1,1)

r5.dup <- replicate(10,
                {
                  rating5
                })

ru <- matrix(rowSums(r5.dup), ncol=6)

colnames(ru) <- c("No attention in the platform","Lack Of fund","Project Mngmnt Skills","User loyalty",
                  "Not getting right human resources",
                  "Lack of knowledge in branding and promotion")

rownames(ru) <- "Respondents"


barplot(as.vector(ru) , main="What makes the your app unique: 5-Rating",col = c("blue","red","orange","green",'purple','yellow'),
        names.arg = c("No attention in the platform","Lack Of fund","Project Mngmnt Skills","User loyalty",
                      "Not getting right\n human resources",
                      "Lack knowledge \n in branding/promo") )

summary(ru)


###

rating4 <- c(1,3,0,3,3,4)

r4.dup <- replicate(10,
                    {
                      rating4
                    })



r4 <- matrix(rowSums(r4.dup), ncol=6)

colnames(r4) <- c("No attention in the platform","Lack Of fund","Project Mngmnt Skills","User loyalty",
                  "Not getting right human resources",
                  "Lack of knowledge in branding and promotion")
rownames(r4) <- "Respondents"


barplot(as.vector(r4) , main="What makes the your app unique: 4-Rating",col = c("blue","red","orange","green",'purple','yellow'),
        names.arg = c("No attention in the platform","Lack Of fund","Project Mngmnt Skills","User loyalty",
                      "Not getting right\n human resources",
                      "Lack knowledge \n in branding/promo") )


### r3


rating3 <- c(2,1,5,1,2,2)

r3.dup <- replicate(10,
                    {
                      rating3
                    })



r3 <- matrix(rowSums(r3.dup), ncol=6)

colnames(r3) <- c("No attention in the platform","Lack Of fund","Project Mngmnt Skills","User loyalty",
                  "Not getting right human resources",
                  "Lack of knowledge in branding and promotion")
rownames(r3) <- "Respondents"


barplot(as.vector(r3) , main="What makes the your app unique: 3-Rating",   col = c("blue","red","orange","green",'purple','yellow'),
        names.arg = c("No attention in the platform","Lack Of fund","Project Mngmnt Skills","User loyalty",
                      "Not getting right\n human resources",
                      "Lack knowledge \n in branding/promo") )

#rating 2

rating2 <- c(1,0,1,3,0,0)

r2.dup <- replicate(10,
                    {
                      rating2
                    })



r2 <- matrix(rowSums(r2.dup), ncol=6)

colnames(r2) <- c("No attention in the platform","Lack Of fund","Project Mngmnt Skills","User loyalty",
                  "Not getting right human resources",
                  "Lack of knowledge in branding and promotion")
rownames(r2) <- "Respondents"

summary(r2)

barplot(as.vector(r2) , main="What makes the your app unique: 2-Rating",  col = c("blue","red","orange","green",'purple','yellow'),
        names.arg = c("No attention in the platform","Lack Of fund","Project Mngmnt Skills","User loyalty",
                      "Not getting right\n human resources",
                      "Lack knowledge \n in branding/promo") )


#rating 1

rating1 <- c(0,0,1,1,2,1)

r1.dup <- replicate(10,
                    {
                      rating1
                    })



r1 <- matrix(rowSums(r1.dup), ncol=6)

colnames(r1) <- c("No attention in the platform","Lack Of fund","Project Mngmnt Skills","User loyalty",
                  "Not getting right human resources",
                  "Lack of knowledge in branding and promotion")
rownames(r1) <- "Respondents"


barplot(as.vector(r1) , main="What makes the your app unique: 1-Rating",  col = c("blue","red","orange","green",'purple','yellow'),
        names.arg = c("No attention in the platform","Lack Of fund","Project Mngmnt Skills","User loyalty",
                      "Not getting right\n human resources",
                      "Lack knowledge \n in branding/promo") )


par(mfrow=c(3,2))


No.attention <- replicate(10,
                    {
                       c(5,5,5,5,4,3,3,2)
                    })

#No.attention <- table(No.attention)

Lack.of.fund <- replicate(10,
                          {
                            c(5,5,5,5,4,4,4,3)
                          })

#Lack.of.fund <- table(Lack.of.fund)

Proj.Mngmt.Skills <- replicate(10,
                          {
                            c(4,4,4,3,2,2,2,1)
                          })

#Proj.Mngmt.Skills <- table(Proj.Mngmt.Skills)

User.loyalty <- replicate(10,
                               {
                                 c(5,3,3,3,3,3,2,1)
                               })

#User.loyalty <- table(User.loyalty)


Right.HR <- replicate(10,
                          {
                            c(5,3,3,3,3,3,2,1)
                          })

#Right.HR <- table(Right.HR)

lack.knowledge.branding.promo <- replicate(10,
                      {
                        c(5,3,3,3,3,3,2,1)
                      })

#lack.knowledge.branding.promo <- table(lack.knowledge.branding.promo)


No.attention <- c(5,5,5,5,4,3,3,2)
Lack.of.fund <- c(5,5,5,5,4,4,4,3)
Proj.Mngmt.Skills <- c(5,3,3,3,3,3,2,1)
User.loyalty <- c(4,4,4,3,2,2,2,1)
Right.HR <- c(5,4,4,4,3,3,1,1)
lack.knowledge.branding.promo <- c(5,4,4,4,4,3,3,1)



n.attn <- table(c(5,5,5,5,4,3,3,2))

no.attention <- sample(c(2:5), 100, replace=TRUE,prob = n.attn/sum(n.attn))


lof <- table(c(5,5,5,5,4,4,4,3))

Lack.of.fund <- sample(c(3:5), 100, replace=TRUE,prob = lof/sum(lof))


pms <- table(c(5,3,3,3,3,3,2,1))

Proj.Mngmt.Skills <- sample(c(1,2,3,5), 100, replace=TRUE,prob = pms/sum(pms))


ul <- table(c(4,4,4,3,2,2,2,1))

User.loyalty <- sample(c(1:4), 100, replace=TRUE,prob = ul/sum(ul))


rhr <- table(c(5,4,4,4,3,3,1,1))
Right.HR <- sample(c(1,3,4,5), 100, replace=TRUE,prob = rhr/sum(rhr))


lbp <- table(c(5,4,4,4,4,3,3,1))

lack.knowledge.branding.promo <-sample(c(1,3,4,5), 100, replace=TRUE,prob = lbp/sum(lbp))

cgrp <- data.frame(cbind(no.attention,Lack.of.fund,Proj.Mngmt.Skills,User.loyalty,Right.HR,lack.knowledge.branding.promo))


stacked.grp <- stack(cgrp)
head(stacked.grp)
summary(cgrp)

plot(values~ind, data=stacked.grp, ylab = "Rating", xlab=" Top challenges of gaming company")

barplot(colMeans(cgrp[sapply(cgrp, is.numeric)]),col=heat.colors(6), ylim=c(0,5),ylab="Ratings", 
        xlab="ASO strategies", main="Mean of the top challenges of a gaming company")



anova.result <- aov(values ~ ind, data=stacked.grp)

summary(anova.result)

#percentage is organic install

organic <-c(100,100,90,90,90,80,70,60,40,0)

organic.ins <- replicate(10,
                {
                  organic
                })

u <- matrix(rowSums(u1), ncol=6)

colnames(u) <- c("Graphics", "Difficulty","User Interaction","Fun","Design","Others")
rownames(u) <- "Respondents"
pal <- colorRampPalette(colors = c("lightblue", "blue"))(6)


colnames(u) <- c("Graphics", "Difficulty","User Interaction","Fun","Design","Others")
rownames(u) <- "Respondents"
pal <- colorRampPalette(colors = c("lightblue", "blue"))(6)

barplot(as.vector(u) , main="What makes the your app unique",  col = heat.colors(6),
        names.arg = c("Graphics", "Difficulty","User Interaction","Fun","Design","Others") )

org <- table(c(0,40,60,70,80,90,90,90,100,100))

org.ins <- sample(c(0,40,60,70,80,90,100), 100, replace=TRUE,prob = org/sum(org))

plot(table(org.ins), type="o", main="Organic install percentage of the app", ylab="Respondents",xlab="Percentage"
        ,col= "blue")
a <- mean(org.ins)
abline(v=a,  col = 'purple', lwd = 2,lty=2)
legend(x = 40, y = 25, col = 'purple', lwd = 2, lty = 2,
       bty = 'n', cex = 0.9,
       'Mean Organic\ninstalls')


## Do you use ASO techniques to increase your visibility on Google Play or iOS?

ASO.user <- c("Yes","Yes","Yes","Yes","Yes","Yes","Yes","Yes","Yes","No")

at <- table(ASO.user)

a.user <- sample(c("No","Yes"), 100, replace=TRUE,prob = at/sum(at))

barplot(table(a.user), main="Use ASO techniques to increase your visibility on Google Play or iOS", ylab="Respondents"
        ,xlab="Answer"
        ,col= c("red","blue"))


## start applying ASO to your game app?

aso.start <- c("During app planning and creation","During app planning and creation","During app planning and creation",
              "Before release to the app platforms","Before release to the app platforms","Before release to the app platforms",
              "Before release to the app platforms","After release, then we do optimisation","After release, then we do optimisation"
              ,"After release, then we do optimisation")

as <- table(aso.start)


a.start <- sample(c("During app planning\n and creation","Before release to\n the app platforms","After release, then\n we do optimisation"),
                  100, replace=TRUE,prob = as/sum(as))

barplot(table(a.start), main="When do users start applying ASO to your game app?", ylab="Respondents"
        ,xlab="Answers"
        ,col= heat.colors(3), ylim=c(0,40))

table(a.start)/sum(table(a.start))*100

##How do you get ASO done

aso.done <- c("We do it ourselves","We do it ourselves","We do it ourselves","We do it ourselves","We do it ourselves"
              ,"We purchase ASO from a consultant","We use ASO tools w/ advertisements","We use ASO tools w/ advertisements"
              ,"We use ASO tools w/ advertisements","We use ASO tools w/ advertisements")

ad <- table(aso.done)

a.done <- sample(c("We do it ourselves","We purchase ASO from a consultant","We use ASO tools w/ \nadvertisements"),
                  100, replace=TRUE,prob = ad/sum(ad))

barplot(table(a.done), main="How do you get ASO done?", ylab="Respondents"
        ,xlab="Answers"
        ,col= heat.colors(3))


table(a.done)/sum(table(a.done))*100

#What kind of lift do you get from app store optimization?


aso.lift <- table(c(0,20,30,40,40,50,50,50,60,90))
a.lift <- sample(c(0,20,30,40,50,60,90), 100, replace=TRUE,prob = aso.lift/sum(aso.lift))

m.lift <- mean(a.lift)
plot(table(a.lift), type="o", main="What kind of lift do you get from app store optimization?", ylab="Respondents"
        ,xlab="Answers"
        ,col= "blue",xlim=c(0,100) )

abline(v=m.l,  col = 'purple', lwd = 2,lty = 3)
legend(x = 70, y = 25, col = 'purple', lwd = 2, lty = 2,
       bty = 'n', cex = 0.9,
       'Mean lift\nwhen using ASO')


##Using ASO techniques, what is your app abandonment rate after 1 week?

aso.abnd <- table(c(20,40,50,50,50,50,60,70,80,90))
a.abnd <- sample(c(20,40,50,60,70,80,90), 100, replace=TRUE,prob = aso.abnd/sum(aso.abnd))

m.l.abdn <- mean(a.abnd)
plot(table(a.abnd), type="o", main="What is your app abandonment rate after 1 week after using ASO?", ylab="Respondents"
     ,xlab="Answers"
     ,col= "blue",xlim=c(0,100) )

abline(v=m.l,  col = 'purple', lwd = 2,lty = 3)
legend(x = 70, y = 25, col = 'purple', lwd = 2, lty = 2,
       bty = 'n', cex = 0.9,
       'Mean after ASO\nabandonment')

avl <- data.frame(cbind(m.lift,m.l.abdn ))

avl <- data.frame(cbind(a.lift,a.abnd ))

head(avl)
summary(avl)

plot(avl)#, xlim = c(0, 160), ylim = c(0, 170))
abline(coef = c(0, 1), col = 'purple')

grid()

m <- lm(avl$a.abnd ~ avl$a.lift)
m

abline(m, col = 'red')


##  ASO techniques give you edge among other gaming apps

vs <- table(c(5,5,5,5,5,4,3,3,2,1))

visibility <- sample(c(1:5), 100, replace=TRUE,prob = vs/sum(vs))


r.d <- table(c(5,5,4,4,4,4,4,3,3,2))

User.satisfaction <- sample(c(2:5), 100, replace=TRUE,prob = r.d/sum(r.d))


loy <- table(c(5,5,5,4,4,4,3,3,2,2))

loyalty <- sample(c(1,2,3,5), 100, replace=TRUE,prob = loy/sum(loy))


gpc <- table(c(5,5,5,5,4,4,4,4,4,3))

graphics <- sample(c(3:5), 100, replace=TRUE,prob = gpc/sum(gpc))



ASO.cri <- data.frame(cbind(visibility,User.satisfaction,loyalty,graphics))

stacked.aso.grp <- stack(ASO.cri)
head(stacked.aso.grp)
summary(ASO.cri)

#bartlett.test(values~ind, data=stacked.aso.grp)

plot(values~ind, data=stacked.aso.grp, ylab = "Rating", xlab=" ASO techniques for gaming apps")

#get the mean of each column
barplot(colMeans(ASO.cri[sapply(ASO.cri, is.numeric)]),col=heat.colors(4), ylim=c(0,5),ylab="Ratings", 
        xlab="ASO criteria", main="Mean of each ASO Criteria that give edge among other game apps")


am <- lm(values~as.factor(ind), data=stacked.aso.grp)
plot(am)
am.r <- aov(am)
am.r <- aov(values ~ ind, data=stacked.aso.grp)
summary(stacked.asoc.mvmt)

#posttest whch group has different means

TukeyHSD(aov(am.r))

##  ASO techniques make you consistently visible to the customer

cvs <- table(c(5,5,5,4,4,3,3,3,3,1))

visibility <- sample(c(1,3,4,5), 100, replace=TRUE,prob = cvs/sum(cvs))


c.r.d <- table(c(5,5,5,5,4,4,4,4,3,3))

User.satisfaction <- sample(c(3:5), 100, replace=TRUE,prob = c.r.d/sum(c.r.d))


c.loy <- table(c(5,5,5,5,4,4,3,3,3,2))

loyalty <- sample(c(2:5), 100, replace=TRUE,prob = c.loy/sum(c.loy))


c.gpc <- table(c(5,5,5,5,4,4,4,4,4,3))

graphics <- sample(c(3:5), 100, replace=TRUE,prob = c.gpc/sum(c.gpc))




aso.cons <- data.frame(cbind(visibility,User.satisfaction,loyalty,graphics))

stacked.aso.cons <- stack(aso.cons)
summary(aso.cons)

#get the mean of each column
barplot(colMeans(aso.cons[sapply(aso.cons, is.numeric)]),col=heat.colors(4), ylim=c(0,5),ylab="Ratings", 
        xlab="ASO criteria", main="Mean of each ASO Criteria that makes an app consistent")

am <- lm(values~as.factor(ind), data=stacked.aso.cons)
plot(am)
am.r <- aov(am)
am.r <- aov(values ~ ind, data=stacked.aso.cons)

summary(stacked.asoc.mvmt)

#posttest whch group has different means

t <- TukeyHSD(aov(am.r))

par(mar=c(2,11,5,0.5), cex=.7)
plot(t, las=1)

##  ASO factors dramatically show movement (whether decrease or increase) when optimized?

a.name <- table(c(5,5,5,4,4,4,4,4,3,3))

app.name <- sample(c(3:5), 100, replace=TRUE,prob = a.name/sum(a.name))


a.desc <- table(c(5,4,4,4,4,4,3,2))

app.description <- sample(c(2:5), 100, replace=TRUE,prob = a.desc/sum(a.desc))


sc <- table(c(5,5,5,4,4,4,4,2,2,2))

screenshot <- sample(c(2,4,5), 100, replace=TRUE,prob = sc/sum(sc))


vid <- table(c(5,5,5,5,4,4,4,4,4,3))

video <- sample(c(3:5), 100, replace=TRUE,prob = vid/sum(vid))



ic <- table(c(5,5,4,4,4,4,4,4,3,3))

icon <- sample(c(3,4,5), 100, replace=TRUE,prob = ic/sum(ic))


rado <- table(c(5,5,5,5,4,4,4,3,3,3))

ratings.downloads <- sample(c(3:5), 100, replace=TRUE,prob = rado/sum(rado))



ASO.movement <- data.frame(cbind(app.name,app.description,screenshot,video,icon,ratings.downloads))

stacked.asoc.mvmt <- stack(ASO.movement)
summary(ASO.movement)

plot(values~ind, data=stacked.asoc.mvmt, ylab = "Rating", xlab="ASO techniques that cause immediate movement on downloads\n(either increase or decrease)")

#get the mean of each column
barplot(colMeans(ASO.movement[sapply(ASO.movement, is.numeric)]),col=heat.colors(6), ylim=c(0,5),ylab="Ratings", 
        xlab="ASO strategies", main="Mean of ASO techniques that cause immediate movement on downloads\n(either increase or decrease")


analysis.movement <- lm(values~as.factor(ind), data=stacked.asoc.mvmt)
plot(analysis.movement)
anova.result <- aov(analysis.movement)
anova.result <- aov(values ~ ind, data=stacked.asoc.mvmt)
summary(stacked.asoc.mvmt)

#posttest whch group has different means

TukeyHSD(aov(analysis.movement))


# How often do you optimize keywords?

opt <- c("Often", "During release only", "During release only", "During release only"
         ,"According to the keyword trend","According to the keyword trend","According to the keyword trend"
         ,"I seldom update my app's keywords","I seldom update my app's keywords","I seldom update my app's keywords")

opt.aso <- table(opt)

o.aso <- sample(c("Often", "During release only","According to the keyword trend"
                   ,"I seldom update my app's keywords"), 100, replace=TRUE,prob = opt.aso/sum(opt.aso))

barplot(table(o.aso), main="How often do you optimize keywords?", ylab="Respondents"
        ,xlab="Answers"
        ,col= heat.colors(4))

## what is the most challenging to optimize

a.name <- table(c(5,5,5,5,5,4,4,4,3,1))

app.name <- sample(c(1,3,4,5), 100, replace=TRUE,prob = a.name/sum(a.name))


a.desc <- table(c(5,5,5,5,5,4,4,4,4,3))

app.description <- sample(c(3:5), 100, replace=TRUE,prob = a.desc/sum(a.desc))


sc <- table(c(5,4,4,4,3,3,3,2,2,2))

screenshot <- sample(c(2:5), 100, replace=TRUE,prob = sc/sum(sc))


vid <- table(c(5,4,4,4,4,3,3,3,3,3))

video <- sample(c(3:5), 100, replace=TRUE,prob = vid/sum(vid))



ic <- table(c(5,5,4,4,4,4,4,3,3,2))

icon <- sample(c(2:5), 100, replace=TRUE,prob = ic/sum(ic))


rado <- table(c(5,5,5,5,4,4,2,2,2,1))

ratings.downloads <- sample(c(1,2,4,5), 100, replace=TRUE,prob = rado/sum(rado))



ASO.challenging <- data.frame(cbind(app.name,app.description,screenshot,video,icon,ratings.downloads))

stacked.asoc.challenging <- stack(ASO.challenging)
summary(ASO.challenging)

#get the mean of each column
barplot(colMeans(ASO.challenging[sapply(ASO.challenging, is.numeric)]),col=heat.colors(4), ylim=c(0,5),ylab="Ratings", 
        xlab="ASO strategies", main="Mean of Hard to optimize ASO techniques")



## what is the most important to optimize

a.name <- table(c(5,5,5,5,4,4,3,3,2,1))

app.name <- sample(c(1:5), 100, replace=TRUE,prob = a.name/sum(a.name))


a.desc <- table(c(4,4,4,4,4,4,4,3,3,2))

app.description <- sample(c(2:4), 100, replace=TRUE,prob = a.desc/sum(a.desc))


sc <- table(c(5,5,5,5,5,5,4,4,3,2))

screenshot <- sample(c(2:5), 100, replace=TRUE,prob = sc/sum(sc))


vid <- table(c(5,5,5,5,4,4,4,4,4,3))

video <- sample(c(3:5), 100, replace=TRUE,prob = vid/sum(vid))



ic <- table(c(5,5,5,5,5,4,3,3,3,2))

icon <- sample(c(2:5), 100, replace=TRUE,prob = ic/sum(ic))


rado <- table(c(5,5,5,5,4,4,3,3,2,2))

ratings.downloads <- sample(c(2:5), 100, replace=TRUE,prob = rado/sum(rado))



ASO.impt <- data.frame(cbind(app.name,app.description,screenshot,video,icon,ratings.downloads))

stacked.asoc.impt <- stack(ASO.impt)
summary(ASO.impt)

#get the mean of each column
barplot(colMeans(ASO.impt[sapply(ASO.impt, is.numeric)]),col=heat.colors(6), ylim=c(0,5),ylab="Ratings", 
        xlab="ASO strategies", main="Mean of Most important to optimize ASO techniques")


# How often do you optimize graphics?

opt.g <- c("I seldom update my app's keywords","I seldom update my app's keywords",
         "I seldom update my app's keywords","I seldom update my app's keywords",
         "Often", "During release only", "During release only"
         ,"According to the keyword trend","According to the keyword trend","According to the keyword trend")

opt.g.aso <- table(opt.g)

o.aso <- sample(c("According to the keyword trend","During release only",
                  "I seldom update my app's keywords","Often"), 100, replace=TRUE,prob = opt.g.aso/sum(opt.g.aso))

barplot(table(o.aso), main="How often do you optimize graphics?", ylab="Respondents"
        ,xlab="Answers"
        ,col= heat.colors(4))
