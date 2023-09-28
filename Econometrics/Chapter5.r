#����� 5
#��������������������


library("sandwich")
library("lmtest")
library("stargazer") 



#������� ��� ��������� ����������� ������:
#��������� � �������������������� ����������� ������ � ������ ������������� ���:
cse = function(reg) {
  rob = sqrt(diag(vcovHC(reg, type = "HC1")))
  return(rob)
}


#��������� ������ ������
D <- read.csv("Agriculture.csv", sep=";", dec=",", header=TRUE)


#���������� ��� ������ ������� ���
model1 <- lm(data=D, PRODP~FUNG1+FUNG2+YDOB1+YDOB2+GIRB+INSEC+LABOUR)
summary(model1)

stargazer(model1,   
          title="Model1 � �������� ������������ ��������", type="text", 
          column.labels=c("OLS"), 
          df=FALSE, digits=4)

stargazer(model1,   
          se=list(cse(model1)), 
          title="Model1 � ���������� ������������ ��������", type="text", 
          column.labels=c("OLS"), 
          df=FALSE, digits=4)

#���� ������ -- ������ �� ���������� ��������������������
bptest(model1)
