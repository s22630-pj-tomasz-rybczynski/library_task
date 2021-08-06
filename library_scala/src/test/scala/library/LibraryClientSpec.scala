package library

import library.app.LibraryClient
import library.error.LibraryApplicationError.{BookAlreadyLent, BookNotFound}
import library.model.{Book, BookId, Borrower, BorrowerId}
import zio.test.Assertion.equalTo
import zio.test.TestAspect.sequential
import zio.test._

object LibraryClientSpec extends DefaultRunnableSpec {
  val book1 = Book("Tom Jones", "1749", "Henry Fielding")
  val bookId1 = BookId(1)
  val book2 = Book("The Plague", "1947", "Albert Camus")
  val bookId2 = BookId(2)
  val book3 = Book("The Great Gatsby", "1925", "F. Scott Fitzgerald")
  val bookId3 = BookId(3)
  val borrower1 = Borrower("Tom")
  val borrowerId1 = BorrowerId(1)

  override def spec = suite("LibraryClientSpec")(
    testM("should add book to database and list available books") {
      for {
        _ <- LibraryClient.addBook(book1)
        list <- LibraryClient.list(true)
      } yield assert(list)(equalTo(List(book1)))
    },
    testM("should add second book and then remove first book") {
      for {
        _ <- LibraryClient.addBook(book2)
        _ <- assertM(LibraryClient.list(true))(equalTo(List(book1, book2)))
        _ <- LibraryClient.remove(bookId1)
        list <- LibraryClient.list(true)
      } yield assert(list)(equalTo(List(book2)))
    },
    testM("should lend a book to Tom and display details") {
      for {
        _ <- LibraryClient.addBorrower(borrower1)
        _ <- LibraryClient.lend(bookId2, borrowerId1)
        details <- LibraryClient.showDetails(bookId2)
      } yield assert(details)(equalTo(s"$book2 is borrowed by ${borrower1.name}"))
    },
    testM("should add book and display details") {
      for {
        _ <- LibraryClient.addBook(book3)
        details <- LibraryClient.showDetails(bookId3)
      } yield assert(details)(equalTo(s"$book3 is available to lent"))
    },
    testM("should find some book with id = 3") {
      for {
        book <- LibraryClient.find(bookId3)
      } yield assert(book)(equalTo(Some(book3)))
    },
    testM("should not find a book with id = 4") {
      for {
        book <- LibraryClient.find(BookId(4))
      } yield assert(book)(equalTo(None))
    },
    testM("should list unavailable books") {
      for {
        list <- LibraryClient.list(false)
      } yield assert(list)(equalTo(List(book2)))
    },
    testM("should fail with 'BookNotFound' on removing a book that doesn't exist") {
      for {
        error <- LibraryClient.remove(bookId1).flip
      } yield assert(error)(equalTo(BookNotFound))
    },
    testM("should fail with 'BookNotFound' on lending a book that doesn't exist") {
      for {
        error <- LibraryClient.lend(BookId(4), borrowerId1).flip
      } yield assert(error)(equalTo(BookNotFound))
    },
    testM("should fail with 'BookAlreadyLent' on removing a book that is not available") {
      for {
        error <- LibraryClient.remove(bookId2).flip
      } yield assert(error)(equalTo(BookAlreadyLent))
    },
    testM("should fail with 'BookAlreadyLent' on lending a book that is not available") {
      for {
        _ <- LibraryClient.addBorrower(Borrower("John"))
        error <- LibraryClient.lend(bookId2, BorrowerId(2)).flip
      } yield assert(error)(equalTo(BookAlreadyLent))
    }
  ).provideCustomLayerShared(LibraryClient.live) @@ sequential
}
