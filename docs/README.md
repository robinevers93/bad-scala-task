# Expected Findings by Engineer Level

This document outlines what different levels of engineers should identify and address in this debugging exercise.

## Mid-Level Engineer - Expected Findings

A mid-level engineer should be able to identify and fix the following issues:

### Critical Bugs to Fix

1. **`addBookToAuthor` method - Redundant lookup bug**
    - **Location**: `Main.scala` line 28-32
    - **Issue**: The method calls `getByName(author)` which returns `Option[Author]`, then maps to get the ID, then calls `getOpt` with that ID - this is redundant since we already have the author from `getByName`
    - **Expected fix**: Simplify to just check if author exists via `getByName`, then add the book
    - **Example fix**:
      ```scala
      def addBookToAuthor(book: Book, author: String): Option[Book] =
        authorDatabase.getByName(author).map { _ =>
          bookDatabase.add(book)
          book
        }
      ```

2. **`getBook` method - Type mismatch**
    - **Location**: `Main.scala` line 9-11
    - **Issue**: Method signature says `getBook(title: String)` but the database uses `id` as the key, not `title`
    - **Expected fix**: Either change parameter to `id: String` or implement proper lookup by title

3. **`getAuthorTotalPages` return type**
    - **Location**: `Main.scala` line 23-26
    - **Issue**: Returns `Double` but pages are `Int`, and the sum of Ints should be Int
    - **Expected fix**: Change return type to `Int`

### Code Quality Issues

4. **Inconsistent error handling**
    - `getBook` throws exceptions while `getOpt` returns `Option`
    - Should be consistent - prefer `Option` for safer error handling

5. **Inefficient `getByName` lookup**
    - **Location**: `AuthorDatabase.getByName` line 64
    - **Issue**: Does O(n) linear search through all authors
    - **Expected improvement**: Could use a `Map[String, Author]` indexed by name for O(1) lookup

6. **Poor variable naming**
    - `as` for authors map in `AuthorDatabase` is unclear
    - Should use descriptive names like `authors`

### Test Coverage

All tests should pass after fixing the bugs above. The test suite defines the expected behavior.

---

## Staff-Level Engineer - Additional Findings

A staff-level engineer with 15+ years of functional programming experience would identify all of the above, plus:

### Architectural & Design Issues

1. **Mutable state everywhere**
    - All databases use `mutable.Map` instead of immutable data structures
    - Violates functional programming principles
    - **Expected refactor**: Use immutable `Map` and return new instances on mutations

2. **Missing abstractions**
    - No Repository pattern or type classes
    - Databases are just thin wrappers around mutable maps
    - **Expected refactor**: Introduce proper abstractions (traits/interfaces) for repositories

3. **Tight coupling**
    - `LibraryService` is tightly coupled to concrete implementations
    - No dependency injection or proper separation of concerns
    - **Expected refactor**: Use dependency injection, possibly with type classes

### Functional Programming Violations

4. **Not truly functional**
    - Side effects everywhere (mutations)
    - No pure functions
    - **Expected refactor**: Use `cats-effect`, `ZIO`, or at least proper monadic error handling

5. **Inconsistent error handling**
    - Mix of exceptions and `Option`
    - **Expected refactor**: Use `Either[DomainError, T]` or a proper error ADT throughout

6. **Missing type safety**
    - IDs are just `String` with no type safety
    - **Expected refactor**: Use newtypes or value classes: `case class AuthorId(value: String)`

### Data Modeling Issues

7. **Denormalized data model**
    - `Book` stores `authorName: String` instead of `authorId: AuthorId`
    - Breaks referential integrity
    - **Expected refactor**: Use proper foreign key relationships

8. **No validation layer**
    - No input validation
    - No domain error types
    - **Expected refactor**: Add validation and proper error types

### Performance & Scalability

9. **Inefficient queries**
    - Every query loads all books/authors and filters in memory
    - Doesn't scale
    - **Expected refactor**: Proper indexing or query optimization

10. **Test isolation issues**
    - Tests share mutable state
    - **Expected refactor**: Isolated test fixtures, possibly property-based tests

### What Staff Engineers Would Say

A staff engineer would likely comment:

> "I understand this is a debugging exercise, but if you want me to refactor this properly, I'd rewrite it using:
> - Immutable data structures
> - A proper Repository/DAO pattern with type classes
> - `cats-effect` or `ZIO` for effect management
> - `Either` or a custom error ADT for error handling
> - Proper indexing for O(1) lookups
> - Type-safe IDs using newtypes
> - Referential integrity in the data model
> - Isolated, property-based tests
>
> The current code has bugs, but more importantly, it's not maintainable or scalable. Should I fix just the bugs, or do you want a proper functional refactor?"

---

## Assessment Criteria

### For Mid-Level Engineers
- ✅ Can identify and fix the critical bugs
- ✅ Can improve basic code quality (naming, error handling)
- ✅ All tests pass
- ✅ Code is correct and handles edge cases

### For Staff-Level Engineers
- ✅ All of the above, plus:
- ✅ Identifies architectural issues
- ✅ Suggests proper functional programming patterns
- ✅ Considers scalability and maintainability
- ✅ May question if the exercise is appropriate for their level
- ✅ Provides thoughtful critique of the design

---

## Notes for Reviewers

- **Mid-level engineers** should focus on fixing bugs and making tests pass
- **Staff engineers** will likely provide architectural feedback - this is expected and valuable
- The exercise is intentionally designed for mid-level engineers; staff engineers may find it too simple
- Consider the engineer's response style: do they fix bugs pragmatically, or do they want to rewrite everything?