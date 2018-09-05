

# In this file I create lines for param_meas,
# to copy-paste at end of cureent param_meas.csv

vect_I <- c(2,3,4,5,7,10,12,13,15,17,20,25,30)
vect_J <- c(30)
vect_r <- c(0.1,1)

lines <- 
  data.frame(
    expand.grid(I=vect_I,J=vect_J,r=vect_r))

lines$meas_idx <- 65+ 1:nrow(lines)
lines$field_idx <- 1

lines <- lines[,c('meas_idx','field_idx','I','J','r')]

write.table(x = lines,file = 'param_meas_lines.csv',
          quote = F,sep = ";",row.names = F)
