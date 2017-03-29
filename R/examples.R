#my_db <- 'R:/shiny/beama/bmonitor/bss.sqlite'
#my_db <- 'R:/packages/beamafx/inst/extdata/beamafx.sqlite'
#abc <- storedQry::SQ$new(db=my_db)
#abc$tables_list()
#abc$table_tail('fx_data',order_by = 'yr,mth,dy desc')
#abc$set_name('sk_view_group')

# abc$set_params(
#   list(
#     `@s_grp`= 'wma_cons_mth',
#     `@i_yr` = 2016,
#     `@i_mth` = 2
#   )
# )

#abc$qry_exec()
#storedQry::SQ$new( 'R:/packages/beamafx/inst/extdata/beamafx.sqlite' )$set_name("fx_update_periods")$qry_exec()

# abc <- storedQry::SQ$new(
#   'R:/shiny/beama/bmonitor/bss.sqlite'
#   )$set_name(
#     'trends_growth_add_detail'
#   )$set_params(
#     list(
#       `@i_yr` = as.character(2016),
#       `@i_mth` = as.character(6),
#       `@s_code` = 'JQR4',
#       `@s_mom`= "Between May 2016 and June 2016 Building of ships & boats turnover fell by -0.3%.",
#       `@s_yoy` = "Building of ships & boats turnover fell by -29.1%  in the year to June 2016, down from 4.9% in May 2016",
# 
#       `@i_mm` = as.character(-0.3) ,
#       `@i_yy` = as.character(-29.1) ,
#       `@i_mm_sc` = as.character(1),
#       `@i_yy_sc` = as.character(2),
#       `@i_ytd` = as.character(-99999),
#       `@i_value` = as.character(359.5)
# 
#     )
#   )

# library(storedQry)
# SQ$new(
#   'R:/shiny/beama/bmonitor/bss.sqlite'
#  )$set_name(
#    "tdi_add_indicator"
#  )$set_params(
#    list(
#      `@i_yr` = 2016,
#      `@i_mth` = 9,
#      `@i_dy` = 8,
#      `@s_data_code` = 'USD',
#      `@i_data_value` = 1.3359
#    )
#  )$params_replace()

# SQ$new(
#   'R:/shiny/beama/bmonitor/bss.sqlite'
# )$set_name(
#   "trends_update_periods"
# )$params_replace()

# abc <-storedQry::SQ$new( "R:/shiny/beama/bmonitor/bss.sqlite" )$set_name( "tdi_update_indicator" )$set_params(
#     list(
#           `@i_yr` =  2017,
#           `@i_mth` =  3,
#           `@i_dy` =  22,
#           `@s_data_code` =  'USD',
#           `@i_data_value` =  1.2452 ,
#           `@i_data_lowest` =  1.2065,
#           `@i_data_highest` = 2.1064
#     )
# )
# 
# abc$params_replace()
# 
# cde <- SQ$new( "R:/shiny/beama/bmonitor/bss.sqlite" )$set_name( "tdi_update_indicator" )$set_params(
#   list(
#     `@i_yr` =  2017,
#     `@i_mth` =  3,
#     `@i_dy` =  22,
#     `@s_data_code` =  'USD',
#     `@i_data_value` =  1.2452 ,
#     `@i_data_lowest` =  1.2065,
#     `@i_data_highest` = 2.1064
#   )
# )
# cde$params_replace()
