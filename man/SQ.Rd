\name{SQ}
\alias{SQ}
\title{R6 Class For Managing Queries Stored in SQLite database}

\usage{

    SQ$new( db )$table_procreate()  #create stored_queries table


    #set query name and parameters
    SQ$new( db, name , params)
    SQ$new( db, name )$set_params( params )
    SQ$new( db )$set_name( name )$set_params( params )

    #set query name, parameters and execute/add/update/delete/list/print
    SQ$new( db, name , params )$qry_exec()
    SQ$new( db, name , params )$qry_add()
    SQ$new( db, name , params )$qry_update()
    SQ$new( db, name , params )$qry_delete()
    SQ$new( db, name , params )$qry_list()
    SQ$new( db, name , params )$qry_print_sql()

    SQ$new( db )$qry_list()   # list names of all stored queries
    SQ$new( db )$table_list() # list tables in database db
    SQ$new( db )$table_count()

    #operations on tables stored in db
    SQ$new( db )$table_get_records(tname, top=TRUE, where=NULL, order_by = order_by )
    SQ$new( db )$table_head(tname, n = 10, where = NULL, order_by = order_by )
    SQ$new( db )$table_tail(tname, n = 10, where = NULL, order_by = order_by )
    SQ$new( db )$table_all(tname, where= NULL, order_by = order_by )
    SQ$new( db )$table_info(tname)
    SQ$new( db )$table_add(tname, df)
    SQ$new( db )$table_create(tname, df)
    SQ$new( db )$table_update(tname, set, where)
    SQ$new( db )$table_insert(tname, fields, values)
    SQ$new( db )$table_select(tname, fields, where,n )
}
\arguments{
  \item{name}{ name of the query to execute}
  \item{params}{ list of query parameters. The parameter names are surrounded by backticks and are prepended by \code{@i_} for \code{numbers} or \code{@s_} for \code{strings} parameters. Example: \cr
    \code{
      list(
        `@s_name` = "Emmalyn",
        `@i_age` = 5,
        `@i_weight` = 2.5
      )
     }
    }
  \item{db}{ name of the sqlite database. The queries are expected to be stored in table named \code{stored_queres} in database \code{db} }
  \item{tname}{ name of the table}
  \item{top}{ logical variable to indicate whether top or bottom records to be retrieved}
  \item{where}{ sql where clase for subsetting records to be returned}
  \item{n}{ number of records to return}
  \item{df}{ dataframe to be stored or appended}
  \item{fields}{ a vector containing the fields of the tables to be queries}
  \item{values}{ a vector containing values to be added to the table}
}
\description{

SQ provides a convienient way to separate complex sql statements from R code.
SQ class is a helper class and not intended to run on its own. SQ expects a table by name \code{stored_queries} to exist in sqlite database \code{db}.

The ddl statement of the \code{stored_queries} table is as follows

\preformatted{
 CREATE TABLE stored_queries (
      id    INTEGER      PRIMARY KEY AUTOINCREMENT NOT NULL,
      qry_name           VARCHAR (50),
      qry_params         TEXT,
      qry_sql            TEXT,
      qry_default_values TEXT
      );
 }

 Using the \code{procreate} method of class \code{SQ} will run the above ddl and create the query table if does not already exist.

}
\examples{

 ##Not run
  sq <- SQ$new( 'my_db', 'my_qry_name', list(`@s_field1`) = "value1", `@i_field2` = 2, ... ) )
  sq$qry_exec()

  #or
  SQ$new( 'my_db' )$set_name( 'my_qry_name' )$set_params( list(`@s_field1`) = "value1", `@i_field2` = 2, ... ) )$qry_exec()

}
