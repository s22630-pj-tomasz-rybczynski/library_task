package library.app

import library.error.LibraryApplicationError
import library.error.LibraryApplicationError._
import library.model.{Book, BookId, Borrower, BorrowerId}
import zio._

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.concurrent.TrieMap

object LibraryClient {

  type LibraryClient = Has[Service]

  private val bookDatabase: TrieMap[BookId, Book] = TrieMap[BookId, Book]()
  private val borrowersDatabase: TrieMap[BorrowerId, Borrower] = TrieMap[BorrowerId, Borrower]()
  private val bookToBorrowers: TrieMap[BookId, BorrowerId] = TrieMap[BookId, BorrowerId]()

  trait Service {
    def addBook(book: Book): UIO[Unit]

    def addBorrower(borrower: Borrower): UIO[Unit]

    def remove(id: BookId): IO[LibraryApplicationError, Unit]

    def list(isAvailable: Boolean): UIO[List[Book]]

    def find(id: BookId): UIO[Option[Book]]

    def lend(bookId: BookId, borrowerId: BorrowerId): IO[LibraryApplicationError, Unit]

    def showDetails(id: BookId): IO[LibraryApplicationError, String]
  }

  def addBook(book: Book): URIO[LibraryClient, Unit] =
    ZIO.accessM(_.get.addBook(book))

  def addBorrower(borrower: Borrower): URIO[LibraryClient, Unit] =
    ZIO.accessM(_.get.addBorrower(borrower))

  def remove(id: BookId): ZIO[LibraryClient, LibraryApplicationError, Unit] =
    ZIO.accessM(_.get.remove(id))

  def list(isAvailable: Boolean): URIO[LibraryClient, List[Book]] =
    ZIO.accessM(_.get.list(isAvailable))

  def find(id: BookId): URIO[LibraryClient, Option[Book]] =
    ZIO.accessM(_.get.find(id))

  def lend(bookId: BookId, borrowerId: BorrowerId): ZIO[LibraryClient, LibraryApplicationError, Unit] =
    ZIO.accessM(_.get.lend(bookId, borrowerId))

  def showDetails(id: BookId): ZIO[LibraryClient, LibraryApplicationError, String] =
    ZIO.accessM(_.get.showDetails(id))

  def live: ULayer[Has[Service]] = ZLayer.succeed {
    new Service {
      val bookIdCounter = new AtomicInteger
      val borrowerIdCounter = new AtomicInteger

      override def addBook(book: Book): UIO[Unit] =
        IO.succeed(bookDatabase.addOne((BookId(bookIdCounter.incrementAndGet()), book)))

      override def addBorrower(borrower: Borrower): UIO[Unit] =
        IO.succeed(borrowersDatabase.addOne((BorrowerId(borrowerIdCounter.incrementAndGet()), borrower)))

      override def remove(id: BookId): IO[LibraryApplicationError, Unit] =
        if (bookToBorrowers.contains(id))
          IO.fail(BookAlreadyLent)
        else
          bookDatabase.remove(id) match {
            case Some(_) => IO.unit
            case None => IO.fail(BookNotFound)
          }

      override def list(isAvailable: Boolean): UIO[List[Book]] =
        if (isAvailable)
          UIO.succeed(bookDatabase.filterNot(bookWithId => bookToBorrowers.contains(bookWithId._1)).values.toList)
        else
          UIO.succeed(bookDatabase.filter(bookWithId => bookToBorrowers.contains(bookWithId._1)).values.toList)

      override def find(id: BookId): UIO[Option[Book]] = UIO.succeed(bookDatabase.get(id))

      override def lend(bookId: BookId, borrowerId: BorrowerId): IO[LibraryApplicationError, Unit] =
        (bookDatabase.get(bookId), borrowersDatabase.get(borrowerId), bookToBorrowers.contains(bookId)) match {
          case (_, _, true)              => IO.fail(BookAlreadyLent)
          case (_, None, _)              => IO.fail(BorrowerNotFound)
          case (None, _, _)              => IO.fail(BookNotFound)
          case (Some(_), Some(_), false) => IO.succeed(bookToBorrowers.addOne((bookId, borrowerId)))
        }

      override def showDetails(id: BookId): IO[LibraryApplicationError, String] =
        for {
          optionBook <- find(id)
          book <- IO.fromOption(optionBook).orElseFail(BookNotFound)
        } yield {
          if (bookToBorrowers.contains(id))
            s"$book is borrowed by ${borrowersDatabase(bookToBorrowers(id)).name}"
          else
            s"$book is available to lent"
        }
    }
  }
}
