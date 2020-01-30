df = read_excel("Desktop/Foundations of business analytics/Assignment-2/graduate-admissions/Grad_student.xls")

head(df)
str(df)

#Histogram of GRE Scores
pl <- ggplot(df,aes(x=GREScore,fill = ))
#pl1 = pl + geom_histogram(binwidth = 3,color = 'black',fill = 'light blue',alpha = 0.6,linetype="dashed")

pl1 = pl + geom_histogram(binwidth = 3,aes(fill = ..count.. ))
pl1

pl2 = pl1 + xlab('Range of GRE Score') + ylab('Frequency of the score')
pl2

print(pl2 + ggtitle('Distribution Of GRE Score') + theme(plot.title = element_text(hjust = 0.5))) 

#Scatter plot

p = ggplot(df,aes(x=GREScore,y=ChanceOfAdmit))
p1 = p + geom_point(aes(size = TOEFLScore))

p2 = p1 + ggtitle('GRE Score vs Chances Of Admits') + theme(plot.title = element_text(hjust = 0.5))
p2

#Bar plot

a = ggplot(df,aes(x=Abr))
print(a + geom_bar(aes(fill = factor(UniversityRating))) + ggtitle('Range of University Ratings') + theme(plot.title = element_text(hjust = 0.5)) + xlab('ABBREVIATIONS') + ylab('Range of Ratings'))

#Boxplot

x = ggplot(df, aes(x=Abr, y = CGPA))

y = x + geom_boxplot(aes(fill = factor(Abr)))
y

#correlation
attach(df)
cor.test(ChanceOfAdmit,SOP,method=c("pearson"))
cor.test(ChanceOfAdmit,GREScore,method=c("pearson"))
cor.test(ChanceOfAdmit,TOEFLScore,method=c("pearson"))
cor.test(ChanceOfAdmit,LOR,method=c("pearson"))
cor.test(ChanceOfAdmit,CGPA,method=c("pearson"))
cor.test(ChanceOfAdmit,Research,method=c("pearson"))

#Regression

model = lm(ChanceOfAdmit ~ GREScore + TOEFLScore+ SOP + LOR + CGPA)
summary(model)

summary(df)
