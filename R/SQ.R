SQ <- R6::R6Class(
  "SQ",
  inherit = SQ_utils,

  public = list(

    params = NULL,        # query parameters
    name = NULL,          # query name
    qry_sql = NULL,    # stored query sql statement
    qry_params = NULL, # stored query parameters


    initialize = function(db, name , params) {

      super$initialize(db)

      if(!is.null(self$db)){

          #set stored query name & parameters if supplied
          if (!missing(name) && !is.null(name)) {

            if (self$qry_exists(name)) {
               self$set_name()
            }else{
               stop(paste0("Query ",name," does not exists"))
            }

          }

          if (!missing(params) && !is.null(params)) {
            self$params <- params
          }
      }else{

        cat('Please supply db path')
        return(NULL)

      }

    },

    dbs_get_path = function() {
      private$get_db()
    },

    #set name
    set_name = function(value) {

      if ( !missing( value ) && !is.null( value ) ) {

        if (self$qry_exists(value)) {

          self$name <- value

          temp <- private$qry_get()

          if (!is.null(temp)) {

            if (nrow(temp == 1)) {

              self$qry_sql <- temp$qry_sql
              self$qry_params <- gsub( "[[:space:]]", "", temp$qry_params )
              invisible(self)

            }else{

              stop("Multiple queries with same name.")

            }
          }

        }else{

          stop(paste0("Query ",value," does not exists"))

        }
      }
      invisible(self)
    },

    get_name = function() {

      return(self$name)
    },


    #set params
    set_params = function(value) {
      if (!missing(value) && !is.null(value) ) {
        self$params <- value
      }
      invisible(self)
    },

    get_params = function() {
      return(
        strsplit(
          self$qry_params,
          private$params_delimeter
         )[[1]]
      )
    },

    #get query name



    #get stored
    qry_get_sql = function() {
      return(self$qry_sql)
    },


    qry_print_sql = function(value) {
      if (missing(value)) {
        cat(self$qry_get_sql())
      }else{
        myqry <-
          paste0("select qry_sql from stored_queries where qry_name='",value,"'")
        temp <- private$run_sql(myqry)
        if (nrow(temp) > 0) {
          cat(temp$qry_sql)
        }else{

        }
      }
      invisible(self)
    },

    params_split = function(value) {
      temp <- base::gsub(",","','",value)
      temp <- base::paste0("('",temp,"')")
      return(temp)
    },

    params_replace = function() {
      self$qry_replace_params()
    },

    qry_replace_params = function() {

      temp_sql <- self$qry_get_sql()

      if ( is.null( temp_sql ) || is.na( temp_sql ) ) {
        stop("No sql found")
      }

      p <- self$get_params()
      kwasi_mahuwo <- "Starting:\n"

      if (!is.na(p) || is.null(p)) {


        for (i in 1:length(p)) {

          kwasi_mahuwo <- sprintf("%s %s = %s \n", kwasi_mahuwo, p[ i ], self$params[[ p[i] ]] )


          if (stringr::str_detect(p[i],"@s_")) {

            temp_sql <- stringr::str_replace(
              temp_sql, p[i], sprintf( "'%s'", self$params[[ p[i] ]])
            )

          }else if (stringr::str_detect(p[i],"@ls_")) {

            temp_sql <- stringr::str_replace(
              temp_sql, p[i], self$params_split( self$params[[ p[i] ]] )
            )

          }else if (stringr::str_detect(p[i],"@li_")) {

            temp_sql <- stringr::str_replace(
              temp_sql, p[i],  sprintf( "(%s)", self$params[[ p[i] ]])
            )

          }else{

            temp_sql <- stringr::str_replace( temp_sql, p[i], self$params[[ p[i] ]] )
          }
        }
      }

      #multi-statement break into vector
      #cat(temp_sql)

      is_multi <- try( stringr::str_detect(temp_sql,";"), silent = T)

      if(class( is_multi ) == 'try-error' || is.na( is_multi) || is.null( is_multi) || length( is_multi) == 0 ){

        cat("Error encountered! Aborting current process ....\n")
        return( kwasi_mahuwo )

      }

      if (is_multi ) {
        temp_sql <- gsub("[\r\n]","",strsplit(temp_sql,";")[[1]])
      }

      return(temp_sql)
    },

    qry_exec = function() {
      temp <- self$qry_replace_params()
      #cat(temp)
      if ( length( temp ) == 1) {
        return( private$run_sql( temp ))
      }else{
        ltemp <- sapply(temp, private$run_sql)
        return(ltemp)
      }

    },


    get_delimiter = function() {
      return (private$params_delimeter)
    },

    set_delimiter = function(value) {
      if (!missing(value)) {
        if (class(value) == 'character' && nchar(value) == 1) {
          private$params_delimeter <- value
          invisible(self)
        }else{
          stop("Delimiter should be single character")
        }

      }else{
        stop("Delimiter cannot be empty")
      }
    },

    qry_add = function(sname,sqry,sparam) {
      if (!missing(sname) && !missing(sqry)) {
        if (missing(sparam)) {
          if (stringr::str_detect(sqry,"@")) {
            stop("Missing parameters")
          }else{
            private$run_sql(
              paste0(
                "insert into stored_queries (qry_name,qry_sql) values ('",
                sname, "','",
                sqry,  "')"
              )
            )
            invisible(self)
          }#@
        }else{
          private$run_sql(
            paste0(
              "insert into stored_queries (qry_name,qry_sql,qry_params) values ('",
              sname, "','",
              sqry,  "','" ,
              sparam,"')"
            )
          )
          invisible(self)
        }
      }else{
        stop("Missing query name or sql statement")
      }#missing
    },#function

    qry_update = function(sname,sqry,sparam) {
      if (!missing(sname)) {
        if (missing(sparam) && !missing(sqry)) {
          private$run_sql(
            paste0(
              " update stored_queries set ",
              " qry_sql='",sqry, "' where ",
              " qry_name='",sname,"';"
            )
          )
          invisible(self)

        }else if (!missing(sparam) && missing(sqry)) {
          private$run_sql(
            paste0(
              " update stored_queries set ",
              " qry_params='",sparam, "' where ",
              " qry_name='",sname,"';"
            )
          )
          invisible(self)

        }else if (!missing(sparam) && !missing(sqry)) {
          private$run_sql(
            paste0(
              " update stored_queries set ",
              " qry_sql='",sqry, "', ",
              " qry_params='",sparam,  "' where ",
              " qry_name='",sname,"';"
            )
          )
          invisible(self)

        }else{
          stop("Missing sql statement or parameters")
        }#missing
      }else{
        stop("Missing stored query name")
      }
    },#function

    qry_delete = function(value) {
      if (!missing(value)) {
        private$run_sql(paste0("delete from stored_queries where qry_name='",value,"';"))
        invisible(self)
      }else{
        stop("Please supply Query name")
      }
    },

    qry_list = function() {
      private$run_sql("select qry_name from stored_queries order by qry_name")
    },

    qry_exists = function(value) {

      if(!missing(value) && !is.null(value)){

          my_data <- private$run_sql(
            paste0(
              "select count(qry_name) as scount from stored_queries where qry_name='",value,"'"
            )
          )

          return(as.numeric(my_data["scount"]) > 0)

      }else{
          cat(paste0(value, " is not valid query name"))
          return(NULL)
      }
    },


    table_import = function(src,dst,tbl) {
      dbh <- RODBC::odbcDriverConnect(src)

      src_table <- RODBC::sqlQuery(dbh,paste0("select * from ",tbl))
      dst_db <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = dst)
      RSQLite::dbWriteTable(conn = dst_db, name = tbl, value = src_table)

      RODBC::close(dbh)
    },

    tables_list = function() {
      tbls <- private$run_sql(paste0("SELECT name FROM sqlite_master WHERE type = 'table'"))
      return(tbls)
    },

    tables_count = function() {
      tbls <- self$tables_list()
      return(nrow(tbls))
    },

    table_get_records = function(tname,n,top = TRUE,where=NULL, order_by=NULL) {
      if (!missing(tname)) {
        temp <- NULL
        if(is.null(order_by)){
            if(is.null(where)){
              temp <- private$run_sql(paste0("select * from ",tname))
            }else{
              temp <- private$run_sql(
                paste0(
                  "select * from ",tname,
                  " where ",where
                )
              )
            }
        }else{
          if(is.null(where)){
            temp <- private$run_sql(
                    paste0(
                      "select * from ",tname,
                      " order by ", order_by
                    )
              )

          }else{

            temp <- private$run_sql(
              paste0(
                "select * from ",tname,
                " where ",where,
                " order by ",order_by
              )
            )
          }

        }
        if (!missing(n)) {
          if (top) {
            return(head(temp,n))
          }else{
            return(tail(temp,n))
          }
        }
        return(temp)
      }else{
        stop("Please supply the table name")
      }
    },

    table_head = function(tname, n = 10,where=NULL, order_by = NULL) {
      temp <- self$table_all(tname,where = where, order_by = order_by )
      if (nrow(temp) > 0) {
        return(head(temp,n = n))
      }else{
        return(temp)
      }
    },

    table_tail = function (tname,n = 10,where=NULL, order_by = NULL) {
      temp <- self$table_all(tname,where = where , order_by = order_by )
      if (nrow(temp) > 0) {
        return(tail(temp,n = n))
      }else{
        return(temp)
      }
    },

    table_split_name = function(tname) {
      temp <- tname
      if (stringr::str_detect(tname,"::")) {
        temp <- stringr::str_split(tname,"::")[[1]]
      }
      return(temp)
    },

    table_all = function(tname,where=NULL, order_by = NULL) {
      if (!missing(tname)) {
        return(self$table_get_records(tname = tname,where=where , order_by = order_by ))
      }else{
        stop("Please supply table name")
      }
    },

    table_info = function(tname) {

      if (!missing(tname)) {

        return(
          private$run_sql(
            paste0(
              "PRAGMA table_info('",tname,"')"
            )
          )
        )

      } else {

        stop("Please supply the table name")

      }
    },

    table_add = function(tname,df) {
      self$table_create(tname,df = df)
    },


    table_create = function (tname,df) {
      RSQLite::dbWriteTable(
        conn = private$get_db_con(),
        name = tname,
        value = df
      )

    },

    table_update = function(tname,set,where){

      if (!missing(tname) && !missing(set) && !missing(where)) {


        qry <- paste0(
          " update ", tname,
          " set    ", set,
          " where  ", where
        )

        private$run_sql(qry)

        invisible(self)
      }else{
        stop(" Missing parameter(s): tname/set/where")
      }
    },

    table_insert = function(tname,fields,values){

      if (!missing(tname) && !missing(fields) && !missing(values)) {
        qry <- paste0(
          " insert into ", tname,
          " (", fields,")",
          " values (", values,")"
        )

        private$run_sql(qry)
        #cat(qry)
        invisible(self)
      }else{
        stop(" Missing parameter(s): tname/fields/values")
      }
    },



    table_select = function(tname,fields=" * ",where,n){

      if (!missing(tname)) {

        temp_where <-""
        if( !missing(where)){
          temp_where <- paste0(" where ",where)
        }

        temp_n <- ""
        if(!missing(n)){

          temp_n <- paste0(" limit ",n)
        }

        qry <- paste0(
          " select ",fields," from ", tname, temp_where, temp_n
        )

        return (
          private$run_sql(qry)
        )
        #cat(qry)
      }else{
        stop(" Missing parameter(s): tname")
      }
    }

    ,table_procreate = function() {
      ddl <- "
      CREATE TABLE stored_queries (
      id    INTEGER      PRIMARY KEY AUTOINCREMENT NOT NULL,
      qry_name           VARCHAR (50),
      qry_params         TEXT,
      qry_sql            TEXT,
      qry_default_values TEXT
      );
      "

      do_i_exist <- "SELECT count(name) as tcount FROM sqlite_master WHERE type='table' AND name='stored_queries';"

      tcount <- private$run_sql(do_i_exist)

      if (nrow(tcount) == 0) {
        be_borned <- private$run_sql(ddl)
      }

      invisible(self)
    }


  ),#public

  private = list(

    params_delimeter = "," ,

    qry_get = function() {
      if (!is.null(self$name)) {
        private$run_sql(paste0(
          "select * from stored_queries where qry_name='", self$name ,"'"
        ))
      }else{
        return(NULL)
      }#if
    }#function




)
)

