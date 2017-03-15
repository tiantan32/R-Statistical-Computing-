teeth=read.table("teeth.txt",head=TRUE)
age=read.table("age.txt",head=TRUE)
age_teeth=data.frame(age,teeth$Num_Teeth)
names(age_teeth)=c("ID","Age","Num_Teeth")
write.table(age_teeth,file="age_teeth.txt",quote=FALSE,
                     sep="\t",row.names=FALSE)

