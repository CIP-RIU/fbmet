#  books = file.path(getwd(), list.files("inst/sample", ".csv", full.names = TRUE))

#' combine_books
#'
#' @param books a simple table
#'
#' @return dataframe
#' @export
combine_books <- function(books) {
  n = length(books)
  dat = read.csv(books[1])
  add_locality_name <- function(dat, book) {
    fn = basename(book)
    fn = stringr::str_replace(fn, ".csv", "")
    cbind(Locality = fn, dat)
  }
  dat = add_locality_name(dat, books[1])

  for(i in 2:n){
    fin = read.csv(books[i])
    fin = add_locality_name(fin, books[i])
    dat = rbind(dat, fin)
  }
  dat
}
