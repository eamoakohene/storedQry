SQ_utils <- R6::R6Class(
    'SQ_utils',
    public = list(
      db = NULL,

      initialize = function(db=NULL){
        self$set_db(db)
      }

      ,set_db = function(value){
        if(!missing(value) && !is.null(value)){
          self$db <- value
        }
        invisible(self)
      }

      ,get_db = function(){
        return( self$db )
      }
    ),

    private = list(

        #get connection string
        get_db_con = function(){
            return(
                DBI::dbConnect(RSQLite::SQLite(), dbname= self$get_db() )
            )
        }

        #execute sql query
        ,run_sql = function(qry) {
            return(sqldf::sqldf(qry, dbname= self$get_db() ))
        }

    )

)
