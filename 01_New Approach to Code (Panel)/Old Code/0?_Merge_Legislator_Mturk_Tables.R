

head(mturk_df_dates)



#A left outer join (or simply left join) of df1 and df2
#Return all rows from the left table, and any rows with matching keys from the right table.

mturk_df$wikititle <- mturk_df$Name 

merged_df <- merge(x = mturk_df, y = US_House, by = "wikititle", all.x = TRUE)



# And Creater Dummies 