import scala.collection.mutable

class LibraryService {

  val db = new BookDatabase()
  val db2 = new AuthorDatabase()

  def addBook(book: Book): Unit = {
    db.add(book)
  }

  def getBook(title: String): Book = {
    db.get(title)
  }

  def getBooksForGenre(g: String): List[Book] = {
    val bs = db.list
    val allBook = bs.map(b => db.get(b.id))

    var o = List.empty[Book]
    for (book <- allBook){
      if (o.contains(book)) {
        "the book is already in the list"
      }
      if (book.genre == "genre") {
        o = o :+ book
      }
    }
    if (o.isEmpty) throw new Exception("Empty List")
    return o
  }

  def getBooksForAuthor(a: String): List[Book] = {
    val author = db2.get(a)

    val bs = db.list
    val allBook = bs.map(b => db.get(b.id))

    var o = List.empty[Book]
    for (book <- allBook) {
      if (o.contains(book)) {
        "the book is already in the list"
      }
      if (book.authorName == author.name) {
        o = o :+ book
      }
    }
    return o
  }

  def getAuthorTotalPages(a: String): Double = {
    val author = db2.get(a)

    val bs = db.list
    val allBook = bs.map(b => db.get(b.id))

    var o = List.empty[Book]
    for (book <- allBook) {
      if (o.contains(book)) {
        "the book is already in the list"
      }
      if (book.authorName == author.name) {
        o = o :+ book
      }
    }

    var c = 1
    for (book <- o) {
      c = c + c + book.pages
    }
    return c
  }

  def addBookToAuthor(book: Book, author: String): Option[Book] =
    db2.getOpt(author) match {
      case Some(_) =>
        db.add(book)
        Some(book)
      case None => throw new Exception("Author not found")
    }

}

object LibraryService {
  def apply(db: BookDatabase, db2: AuthorDatabase) = new LibraryService
}

class BookDatabase  {
  val books: mutable.Map[String, Book] = mutable.Map.empty

  def add(a: Book): Book = {
    books += (a.id -> a)
    a
  }

  def get(id: String): Book = books(id)

  def list: List[Book] = books.values.toList
}

class AuthorDatabase {

  val as: mutable.Map[String, Author] = mutable.Map.empty

  def add(a: Author): Author = {
    as += (a.id -> a)
    a
  }

  def get(id: String): Author = getOpt(id).get

  def getOpt(id: String): Option[Author] = as.get(id)

  def list: List[Author] = as.values.toList

  def getByName(name: String): Option[Author] = list.filter(_.name == name).headOption

}

case class Book(id: String, title: String, authorName: String, genre: String, pages: Int)


case class Author(id: String, name: String)
