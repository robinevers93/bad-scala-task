# Bad Scala Task

## Overview

This is a debugging and refactoring exercise designed to assess your Scala programming skills. The codebase contains a
simple library management system with some intentional bugs and code quality issues that need to be identified and
fixed.

## What You Need to Do

1. **Run the tests** to see which functionality is currently failing
2. **Identify the bugs** in the codebase
3. **Fix the issues** to make all tests pass
4. **Improve code quality** where you see opportunities (optional, but appreciated)

## Project Structure

- `src/main/scala/Main.scala` - Contains the main implementation:
    - `LibraryService` - Main service class for library operations
    - `BookDatabase` - In-memory database for books
    - `AuthorDatabase` - In-memory database for authors
    - `Book` and `Author` case classes

- `src/test/scala/Test.scala` - Contains the test suite that defines expected behavior

## Getting Started

### Prerequisites

- Scala 3.3.3
- sbt (Scala Build Tool)
- Java 17 or 21

### Running the Project

**Important:** Make sure you run sbt commands from the **project root directory** (where `build.sbt` is located).

```bash
# Navigate to the project root (if not already there)
cd /path/to/bad-scala-task

# Run all tests
sbt test

# Compile the project
sbt compile
```

## What We're Looking For

We're assessing your ability to:

> **Note for reviewers**: See `docs/expected-findings.md` for detailed expectations by engineer level.
- **Read and understand existing code** - Can you navigate and comprehend the codebase?
- **Debug effectively** - Can you identify logical errors and understand why code isn't working?
- **Work with Scala idioms** - Do you understand Option types, monadic operations, and functional programming concepts?
- **Follow test-driven requirements** - Can you use existing tests to guide your fixes?
- **Write clean, maintainable code** - Can you improve code quality while fixing bugs?

## Expected Outcome

After completing this task:

- ✅ All tests should pass
- ✅ The code should be correct and handle edge cases appropriately
- ✅ The code should follow Scala best practices

## Tips

- Start by running the tests to see what's failing
- Read the test cases carefully - they define the expected behavior
- Pay attention to Option types and how they're being used
- Consider edge cases and error handling

## Time Estimate

This task is designed to take approximately **30 minutes** for a mid-level engineer or **15 minutes** for a senior-level
engineer.

Good luck!