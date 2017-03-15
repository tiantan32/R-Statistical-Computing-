age_teeth=read.table("age_teeth.txt",sep="\t",head=TRUE)
order_mul_fn<-function(table_name,num){
  for (i in num){
    x=table_name[[i]]
    y=order(x)
    table_name=table_name[y,]
  }
  table_name
}

order_fn_2<-function(this_table,num){
  new_table<-this_table[do.call("order_fn",this_table[num]),]
  return(new_table)
}