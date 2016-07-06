library(dplyr)
library(ggplot2)
library(chron)
library(reshape2)
makdata<-read.csv("../data/某超市销售数据.csv",head=T)
head(makdata,15)
summary(makdata)
str(makdata)
#发现不规范信息

#检查缺失值
sum(is.na(makdata))
#更改销售时间格式.Excel预处理，单元格格式
maktime<-as.character(makdata$销售时间)
makdata$销售时间<-strptime(maktime,"%Y%m%d%H%M%S")

#根据summary中发现信息预处理数据
#删除商品名称的空格
makdata$商品名称<-sub("\\s","",makdata$商品名称, fixed = F)
makdata$商品货号<-sub("\\s","",makdata$商品货号,fixed = F)
#无效商品信息处理
invdata1<-subset(makdata,makdata$商品名称=="合计分舍去")
nrow(invdata1)
invdata2<-subset(makdata,makdata$商品名称=="积点印花")
nrow(invdata2)
makdata<-subset(makdata,makdata$商品名称!="合计分舍去"&makdata$商品名称!="积点印花")
summary(makdata)

#合并单订单中重复商品;子数据集1valdata
valdata<-makdata %>%
  select(c(销售数量,销售金额)) %>%
  aggregate(by=list(单据号=makdata$单据号,商品货号=makdata$商品货号),FUN=sum) %>%
  arrange(单据号) %>%
  merge(unique(makdata[,1:5]),by=c("单据号","商品货号"),all=FALSE) %>%
  mutate(销售单价=销售金额/销售数量)
summary(valdata)
#销售金融为负值商品处理,跨订单退货
nrow(subset(valdata,valdata$销售金额<0))
valdata<-subset(valdata,valdata$销售金额>0)


##变量因子化
valdata[,1]<-as.factor(valdata[,1])
valdata[,2]<-as.factor(valdata[,2])
valdata[,5]<-as.factor(valdata[,5])
valdata[,7]<-as.factor(valdata[,7])


#获取为日期对象为周几/月份/季度
valdata<-mutate(valdata,byday=days(销售时间),byweek=weekdays(销售时间),bymonths=months(销售时间),byhours=hours(销售时间))
valdata[,9]<-as.factor(valdata[,9])
valdata[,10]<-factor(valdata[,10],order=TRUE,levels = c("星期一","星期二","星期三","星期四","星期五","星期六","星期日"))#
valdata[,11]<-factor(valdata[,11],order=TRUE,levels = c("十月","十一月","十二月","一月","二月"))
valdata[,12]<-as.factor(valdata[,12])
valdata$销售时间<-as.Date(valdata$销售时间)

##时间序列分析
##时间序列分析
#按日期,日,周，月,小时汇总销售金额数据/订单量（均值）
#只关注趋势增长情况,对数据标准化，消除量纲的影响
smbt<-tapply(valdata$销售金额,valdata$销售时间,sum)
snbt<-tapply(valdata$单据号,valdata$销售时间,length)
#valdata1<-valdata
spbt<-valdata%>%
  group_by(销售时间)%>%
  summarise(订单数=n_distinct(单据号))

sbt<-rbind(smbt,snbt)%>%t()%>%as.data.frame()
names(sbt)<-c("销售金额","商品数按种类")
sbt$时间<-rownames(sbt)
rownames(sbt)<-NULL
sbt$时间<-strptime(sbt$时间,"%Y-%m-%d")

spbt<-valdata%>%
  group_by(销售时间)%>%
  summarise(订单数=n_distinct(单据号))
names(spbt)<-c("时间","顾客数")
sbt<-cbind(sbt,spbt[,2])
str(sbt)
#
head(sbt,5)
sbt[,c(1,2,4)]<-scale(sbt[,c(1,2,4)])
sbt$时间<-as.Date(sbt$时间)
md<-melt(sbt,id.vars=c("时间"),
         measure.vars=c("销售金额", "商品数按种类", "顾客数"),
         variable.name="项目类型", value.name="值")
x<-md$时间
y<-md$值
z<-md$项目类型
ggplot(md,aes(x=x,y=y,color=z,group=z))+
  geom_line(size=1)+
  geom_point(size=2,shape=21,fill="white")+
  labs(y="标准化后值",x="时间",color="项目类型")+
  theme(legend.position=c(0.5,1),legend.justification=c(0.5,1))+
  ggtitle("按时间销售情况汇总图")
#
smbd<-tapply(valdata$销售金额,valdata$byday,sum)
snbd<-tapply(valdata$单据号,valdata$byday,length)
sbd<-valdata%>%
  select(销售时间,byday)%>%
  unique()%>%
  .$byday%>%
  table()%>%
  rbind(smbd,snbd,.)%>%
  t()%>%
  as.data.frame()
names(sbd)<-c("销售金额","商品数按种类","频数")
sbd<-sbd%>%
  mutate(该号日均销售额=销售金额/频数,该号日均销售商品数=商品数按种类/频数)%>%
  cbind(时间=row.names(.),.)
row.names(sbd)=NULL
spbd<-valdata%>%
  group_by(byday)%>%
  summarise(订单数=n_distinct(单据号))
names(spbd)<-c("时间","顾客数")
sbd<-cbind(sbd,spbd[,2])
sbd$时间<-factor(sbd$时间,order=TRUE,levels = c(1:31))
str(sbd)
#
head(sbd,5)
sbd[,5:7]<-scale(sbd[,5:7])
md<-melt(sbd, id.vars=c("时间"),
         measure.vars=c("该号日均销售额", "该号日均销售商品数", "顾客数"),
         variable.name="项目类型", value.name="值")
x<-md$时间
y<-md$值
z<-md$项目类型
ggplot(md,aes(x=x,y=y,color=z,group=z))+
  geom_line(size=1)+
  geom_point(size=2,shape=21,fill="white")+
  labs(y="标准化后值",x="时间",color="项目类型")+
  theme(legend.position=c(0,1),legend.justification=c(0,1))+
  ggtitle("按日期销售情况汇总图")
#
smbw<-tapply(valdata$销售金额,valdata$byweek,sum)
snbw<-tapply(valdata$单据号,valdata$byweek,length)
sbw<-valdata%>%
  select(销售时间,byweek)%>%
  unique()%>%
  .$byweek%>%
  table()%>%
  rbind(smbw,snbw,.)%>%
  t()%>%
  as.data.frame()
names(sbw)<-c("销售金额","商品数按种类","频数")
sbw$时间<-row.names(sbw)
row.names(sbw)=NULL
sbw<-mutate(sbw,日均销售额=销售金额/频数,日均销售商品数=商品数按种类/频数)
spbw<-valdata%>%
  group_by(byweek)%>%
  summarise(订单数=n_distinct(单据号))
names(spbw)<-c("时间","顾客数")
sbw<-cbind(sbw,spbw[,2])
sbw$时间<-factor(sbw$时间,order=TRUE,levels = c("星期一","星期二","星期三","星期四","星期五","星期六","星期日"))
#
head(sbw,5)
sbw[,5:7]<-scale(sbw[,5:7])
md<-melt(sbw, id.vars=c("时间"),
         measure.vars=c("日均销售额", "日均销售商品数", "顾客数"),
         variable.name="项目类型", value.name="值")
x<-md$时间
y<-md$值
z<-md$项目类型
ggplot(md,aes(x=x,y=y,color=z,group=z))+
  geom_line(size=1)+
  geom_point(size=2,shape=21,fill="white")+
  labs(y="标准化后值",x="时间",color="项目类型")+
  theme(legend.position=c(0,1),legend.justification=c(0,1))+
  ggtitle("按周销售情况汇总图")
#
smbm<-tapply(valdata$销售金额,valdata$bymonths,sum)
snbm<-tapply(valdata$单据号,valdata$bymonths,length)
sbm<-valdata%>%
  select(销售时间,bymonths)%>%
  unique()%>%
  .$bymonths%>%
  table()%>%
  rbind(smbm,snbm,.)%>%
  t()%>%
  as.data.frame()
names(sbm)<-c("销售金额","商品数按种类","频数")
sbm$时间<-row.names(sbm)
row.names(sbm)=NULL
sbm<-mutate(sbm,该月日均销售额=销售金额/频数,该月日均销售商品数=商品数按种类/频数)
spbm<-valdata%>%
  group_by(bymonths)%>%
  summarise(订单数=n_distinct(单据号))
names(spbm)<-c("时间","顾客数")
sbm<-cbind(sbm,spbm[,2])
sbm$时间<-factor(sbm$时间,order=TRUE,levels = c("十月","十一月","十二月","一月","二月"))
#
head(sbm,5)
sbm[,5:7]<-scale(sbm[,5:7])
md<-melt(sbm, id.vars=c("时间"),
         measure.vars=c("该月日均销售额", "该月日均销售商品数", "顾客数"),
         variable.name="项目类型", value.name="值")
x<-md$时间
y<-md$值
z<-md$项目类型
ggplot(md,aes(x=x,y=y,color=z,group=z))+
  geom_line(size=1)+
  geom_point(size=2,shape=21,fill="white")+
  labs(y="标准化后值",x="时间",color="项目类型")+
  theme(legend.position=c(0.5,1),legend.justification=c(0.5,1))+
  ggtitle("按月销售情况汇总图")
#
smbh<-tapply(valdata$销售金额,valdata$byhours,sum)
snbh<-tapply(valdata$单据号,valdata$byhours,length)
sbh<-valdata%>%
  select(销售时间,byhours)%>%
  unique()%>%
  .$byhours%>%
  table()%>%
  rbind(smbh,snbh,.)%>%
  t()%>%
  as.data.frame()
names(sbh)<-c("销售金额","商品数按种类","频数")
sbh$时间<-row.names(sbh)
row.names(sbh)=NULL
sbh<-mutate(sbh,时均销售额=销售金额/频数,时均销售商品数=商品数按种类/频数)
spbh<-valdata%>%
  group_by(byhours)%>%
  summarise(订单数=n_distinct(单据号))
names(spbh)<-c("时间","顾客数")
sbh<-cbind(sbh,spbh[,2])
sbh$时间<-factor(sbh$时间,order=TRUE,levels = c(6:21))
#
head(sbh,5)
sbh[,5:7]<-scale(sbh[,5:7])
md<-melt(sbh, id.vars=c("时间"),
         measure.vars=c("时均销售额", "时均销售商品数", "顾客数"),
         variable.name="项目类型", value.name="值")
x<-md$时间
y<-md$值
z<-md$项目类型
ggplot(md,aes(x=x,y=y,color=z,group=z))+
  geom_line(size=1)+
  geom_point(size=2,shape=21,fill="white")+
  labs(y="标准化后值",x="时间",color="项目类型")+
  theme(legend.position=c(0.5,1),legend.justification=c(0.5,1))+
  ggtitle("按小时销售情况汇总图")



#
##商品信息分析
#分别按总，月，周，时汇总销量前十的商品


#增加销售商品数曲线

##统计商品销售信息
#整体产品销售量
snbp<-valdata%>%
  group_by(商品名称)%>%
  summarise(销售量=n())%>%
  arrange(desc(销售量))
head(snbp,10)
#产品销售量的分布
snbp<-as.data.frame(snbp)
snd<-snbp%>%
  group_by(snbp[,2])%>%
  summarise(商品数=n_distinct(商品名称))
names(snd)<-c("销售量","商品数")

ggplot(snd,aes(snd$销售量,y=snd$商品数))+
  geom_line(size=1)+
  xlim(0,100)+
  ggtitle("不同销售量的商品类型数分布")
###关联规则分析,数据为single
library(arules)

trans<-valdata%>%
  select(单据号,商品名称)%>%
  as("transactions")

itemFrequencyPlot(trans,support=0.004,cex.names=0.6)
#eclat算法，支持度最大的10个项集
fsets<-eclat(trans,parameter = list(support=0.0005,maxlen=10))
inspect(sort(fsets,by="support")[1:10])

#求各种规则，apriori算法
rules<-apriori(trans,parameter = list(support=0.0005,confidence=0.01))
inspect(sort(rules,by="support")[1:3])
inspect(sort(rules,by="lift")[1:3])


