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

# abc <- SQ$new(
#   'R:/shiny/beama/bmonitor/bss.sqlite'
#   )$set_name(
#     'trends_growth_add_detail'
#   )$set_params(
#     list(
#       `@i_yr` = 2016,
#       `@i_mth` = 6,
#       `@s_code` = 'JQR4',
#       `@s_mom`= "Between May 2016 and June 2016 Building of ships & boats turnover fell by -0.3%.",
#       `@s_yoy` = "Building of ships & boats turnover fell by -29.1%  in the year to June 2016, down from 4.9% in May 2016",
#
#       `@i_mm` = -0.3 ,
#       `@i_yy` = -29.1 ,
#       `@i_mm_sc` = 1,
#       `@i_yy_sc` = 2,
#       `@i_ytd` = -99999,
#       `@i_value` = 359.5
#
#     )
#   )


 # SQ$new(
 #   'R:/shiny/beama/bmonitor/bss.sqlite'
 #  )$set_name(
 #    "tdi_update_indicator"
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
