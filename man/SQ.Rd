\name{SQ}
\alias{SQ}
\title{R6 Class For Managing Queries Stored in SQLite database}

\usage{

    SQ$new( db, name , params)
    SQ$new( db, name)$set_params()
    SQ$new( db)$set_name(name)$set_params()

    SQ$new( db, name , params)$qry_exec()
    SQ$new( db, name , params)$qry_add()
    SQ$new( db, name , params)$qry_update()
    SQ$new( db, name , params)$qry_delete()
    SQ$new( db, name , params)$qry_list()
    SQ$new( db, name , params)$qry_print_sql()

    SQ$new( db, name , params)$table_list()
    SQ$new( db, name , params)$table_count()

    SQ$new( db, name , params)$table_get_records(tname, top=TRUE, where=NULL)
    SQ$new( db, name , params)$table_head(tname, n = 10, where = NULL)
    SQ$new( db, name , params)$table_tail(tname, n = 10, where = NULL)
    SQ$new( db, name , params)$table_all(tname, where= NULL)
    SQ$new( db, name , params)$table_info(tname)
    SQ$new( db, name , params)$table_add(tname, df)
    SQ$new( db, name , params)$table_create(tname, df)
    SQ$new( db, name , params)$table_update(tname, set, where)
    SQ$new( db, name , params)$table_insert(tname, fields, values)
    SQ$new( db, name , params)$table_select(tname, fields, where,n )
}
\arguments{
  \item{name}{ name of the query to execute}
  \item{params}{ query parameters}
  \item{db}{ name of the sqlite database. The queries are expected to be stored in \code{stored_queres}}
  \item{tname}{ name of the table}
  \item{top}{ logical variable to indicate whether top or bottom records to be retrieved}
  \item{where}{ sql where clase for subsetting records to be returned}
  \item{n}{ number of records to return}
  \item{df}{ dataframe to be stored or appended}
  \item{fields}{ a vector containing the fields of the tables to be queries}
  \item{values}{ a vector containing values to be added to the table}
}
\description{
SQ class is a helper class and not intended to run on its own. SQ expect a table by name \code{stored_queries} to exist in sqlite database \code{db}.

SQ provide a convienient way to separate sql statements from your R code.

}
\examples{

 ##Not run
  sq <- SQ$new('my_db','my_qry_name','my_qry_params')
  sq$qry_exec()

}
