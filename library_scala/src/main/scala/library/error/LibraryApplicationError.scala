package library.error

sealed trait LibraryApplicationError

object LibraryApplicationError {
  final case object BookNotFound extends LibraryApplicationError
  final case object BookAlreadyLent extends LibraryApplicationError
  final case object BorrowerNotFound extends LibraryApplicationError
}
