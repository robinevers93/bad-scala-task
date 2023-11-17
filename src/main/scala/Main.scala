import scala.collection.mutable

class LibraryService(bookDatabase: BookDatabase, authorDatabase: AuthorDatabase) {

  def addBook(book: Book): Unit = {
    bookDatabase.add(book)
  }

  def getBook(title: String): Book = {
    bookDatabase.get(title)
  }

  def getBooksForGenre(genre: String): List[Book] = {
    val allBooks = bookDatabase.list
    allBooks.filter(_.genre == genre)
  }

  def getBooksForAuthor(authorName: String): List[Book] = {
    val allBooks = bookDatabase.list
    allBooks.filter(_.authorName == authorName)
  }

  def getAuthorTotalPages(authorName: String): Double = {
    val books = getBooksForAuthor(authorName)
    books.map(_.pages).sum
  }

  def addBookToAuthor(book: Book, author: String): Option[Book] =
    authorDatabase.getByName(author).map(_.id).flatMap(authorDatabase.getOpt).map { _ =>
      bookDatabase.add(book)
      book
    }

}

class BookDatabase {
  private val books: mutable.Map[String, Book] = mutable.Map.empty

  def add(a: Book): Book = {
    books += (a.id -> a)
    a
  }

  def get(id: String): Book = books(id)

  def list: List[Book] = books.values.toList
}

class AuthorDatabase {

  private val as: mutable.Map[String, Author] = mutable.Map.empty

  def add(a: Author): Author = {
    as += (a.id -> a)
    a
  }

  def get(id: String): Author = getOpt(id).get

  def getOpt(id: String): Option[Author] = as.get(id)

  def list: List[Author] = as.values.toList

  def getByName(name: String): Option[Author] = list.find(_.name == name)

}

case class Book(id: String, title: String, authorName: String, genre: String, pages: Int)


case class Author(id: String, name: String)
