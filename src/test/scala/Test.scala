import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Test extends AnyWordSpec with Matchers {


  val db = new BookDatabase
  val db2 = new AuthorDatabase
  val libService = new LibraryService

  val a1 = Author("a1", "Jack Smith")
  val a2 = Author("a2", "George Woods")

  db2.add(a1)
  db2.add(a2)

  private val book1: Book = Book("id1", "foo", a1.name, "Action Comedy", 10)
  private val book2: Book = Book("id2", "foo", a2.name, "Horror", 100)
  private val book3: Book = Book("id3", "bar", a1.name, "Horror", 140)
  db.add(book1)
  db.add(book2)
  db.add(book3)

  "getBooksForGenre" must {

    "return an empty list if the genre doesn't exist" in {
      libService.getBooksForGenre("made up ") mustBe List()
    }

    "get the correct books" in {
      libService.getBooksForGenre("Horror") mustBe List(
        book2, book3
      )

      libService.getBooksForGenre("Action Comedy") mustBe List(
        book1
      )
    }
  }
  "getBooksForAuthor" must {

    "return an empty list if the author doesn't exist" in {
      libService.getBooksForAuthor("made up ") mustBe List()
    }
    "get the correct books" in {
      libService.getBooksForAuthor(a1.name) mustBe List(
       book1, book3
      )

      libService.getBooksForAuthor(a2.name) mustBe List(
        book2
      )
    }
  }

  "getAuthorsTotalPages" must {
    "return 0 if no books" in  {
      libService.getAuthorTotalPages("noone") mustBe 0
    }

    "return the expected count" in {
      libService.getAuthorTotalPages(a1.name) mustBe List(book1, book3).map(_.pages).sum
    }
  }

  "addBookToAuthor" must {
    "return None if author doesn't exist" in {
      val a3 = Author("a3", "Nemo")
      libService.addBookToAuthor(Book("id4", "title", a3.name, "Romance", 1), a3.name) mustBe None
    }
    "return the book as a Some(book) if the author exists" in {
      val book = Book("id4", "title", a2.name, "Romance", 1)
      libService.addBookToAuthor(book, a2.name) mustBe Some(book)
    }
  }

  "AuthorDatabase" must {
    "getByName" must {
      "return None if the name doesn't exist" in {
        db2.getByName("no name") mustBe None
      }
    }
  }
}
