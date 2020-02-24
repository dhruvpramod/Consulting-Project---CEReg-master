library(ggplot2)
library(readxl)

#Reading Incident Data
cereg_data<-read.csv("cereg.csv")

IncidentType<- data.frame(table(cereg_data$Incident.Type.s.))
IncidentType<- IncidentType[which(IncidentType$Freq>0),]
ggplot(IncidentType, aes(x = `Var1`, y = `Freq`)) +
  geom_bar(stat="identity",fill = "darkblue") + scale_x_discrete("Incident type") + coord_flip()

WhatH<- data.frame(table(cereg_data$What.Happened.))
WhatH<- WhatH[which(WhatH$Freq>0),]
ggplot(WhatH, aes(x = `Var1`, y = `Freq`)) +
  geom_bar(stat="identity",fill = "darkblue") + scale_x_discrete("What Happened") + coord_flip()

ActivityPerformed<- data.frame(table(cereg_data$Activity.Performed.at.Time.of.Incident))
ActivityPerformed<- ActivityPerformed[which(ActivityPerformed$Freq>0),]
ggplot(ActivityPerformed, aes(x = `Var1`, y = `Freq`)) +
  geom_bar(stat="identity",fill = "darkblue") + scale_x_discrete("Outlet Type")

Conditions<- data.frame(table(cereg_data$Conditions.that.Resulted.in.Operation.Beyond.Limits))
Conditions<- Conditions[which(Conditions$Freq>0),]
ggplot(Conditions, aes(x = `Var1`, y = `Freq`)) +
  geom_bar(stat="identity",fill = "darkblue") + scale_x_discrete("Conditions") + coord_flip()

Substance<- data.frame(table(cereg_data$Substance))
Substance<- Substance[which(Substance$Freq>0),]
ggplot(Substance, aes(x = `Var1`, y = `Freq`)) +
  geom_bar(stat="identity",fill = "darkblue") + scale_x_discrete("Substance Released") + coord_flip()

Province<- data.frame(table(cereg_data$Province))
Province<- Province[which(Province$Freq>0),]
ggplot(Province, aes(x = `Var1`, y = `Freq`)) +
  geom_bar(stat="identity",fill = "darkblue") + scale_x_discrete("Province") + coord_flip()

Company<- data.frame(table(cereg_data$Compay))
Company<- Company[which(Province$Freq>0),]
ggplot(Company, aes(x = `Var1`, y = `Freq`)) +
  geom_bar(stat="identity",fill = "darkblue") + scale_x_discrete("Company Involved") + coord_flip()

