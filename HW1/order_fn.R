age_teeth=read.table("age_teeth.txt",sep="\t",head=TRUE)
order_fn<-function(table_name,num){
  x=table_name[[num]]
  y=order(x)
  table_name[y,]
}



