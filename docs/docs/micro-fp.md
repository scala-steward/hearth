# Micro FP Library

Usually, in macros, we would see on the JVM class path both libraries that we would only need for the compilation,
and libraries that are needed in the runtime.

If our library relied on e.g. Cats in macros, it would not be an issue if macros would use Cats types and methods
in generated code. As a matter of the fact, the dependency would be obvious and required.

But if macros relied on Cats/Scalaz/ZIO/some other big ecosystem, and users would not use it in their code,
that would introduce an unnecessary friction. If they decided to add it, but needed a different major version
(binary incompatibly version) they would be blocked to do so, not due to code that they are controlling, but
because it would be their almost-compile-time-only dependency... they they might not be safe to remove.

Meanwhile, some FP functionalities are very useful for writing macros, so it would be a shame to give up on them
completely.

## Library's Scope

From my experience, when writing macros you may need to:

 1. aggregate errors, so that users would be informed about all the issues preventing from compilation at once
 2. occasionally handle some cases like this:
    ```scala
    '{
      new TypeClass[A] {
        def implementation(arg: A): Result = ${
          generateImpl('{ arg }) // Returns F[Expr[Result]] - how to extract it safely?
        }
      } 
    }
    ```
 3. provide some way of debugging the code - normally, people have logs for that, but with macros they usually do not

Usually the first issue would be solved by Cats users with something like `.parTraverse` and some data type
 that allows error aggregation. `.parTraverse` implies `Traverse` and `Parallel` type classes.
 
Occasionally, one might prefer `.traverse` which adds `Applicative` and all 3 imply `Functor` type class.

And... that's it. That's all Cats-like type classes that the rest of the library rely on. You might add your own,
but the libray aim to stay micro, to **not** encourage using it to replace some full-blown FP library.

The only additional type class is needed to handle the second case:

!!! example "Generic `DirectStyle` type class"

    ```scala
    DirectStyle[F].scoped { runSafe =>
      '{
        new TypeClass[A] {
          def implementation(arg: A): Result = ${
            runSafe(generateImpl('{ arg })) // turns F[Expr[Result]] into Expr[Result]
          }
        } 
      }
    } // : F[Expr[TypeClass[A]]]
    ```

The third issue is addressed by introducing `MIO` (Macro IO) and `Log`.

All of these are opt-in. You don't have to use extension methods if you don't want to. You don't have to use `MIO` and `Log`.

The rest of the library does not require you to write code with IO or type classes. However, it assumes in a few utilities, that
whatever type you would be using for handling errors, would have these type classes provided.

## Type classes

The micro-fp library provides a minimal set of type classes for functional programming patterns commonly needed in macros. All type classes are designed to be lightweight and focused on practical use cases.

### Type Classes Hierarchy

```
Functor
├── Applicative
│   ├── Parallel
│   │   └───────────────────────────────────────────────────────┐
│   └──────────┐                                                │
│              │                                                │        
└── Traverse   │                                                │
    └──────────┤                                                │
               └── ApplicativeTraverse (Applicative + Traverse) │
                   └────────────────────────────────────────────┤
                                                                └── ParallelTraverse (Parallel + Traverse)

DirectStyle
```

### Required Imports

To use the type classes and their syntax, import:

```scala
import hearth.fp.syntax.*
import hearth.fp.instances.*
```

### `Functor`

Represents types that can be mapped over. The `map` operation transforms values inside a context without changing the context structure.

!!! notice "No example"

    Since all structures for which `Functor` is defined have `.map` already implemented as a class method, we'll skip this example.

### `Applicative`

Extends `Functor` with the ability to lift values into a context (`pure`) and combine two contexts with a function (`map2`). Provides sequential, fail-fast semantics.

!!! example "Lifting values with `pure`"

    ```scala
    import hearth.fp.syntax.*
    import hearth.fp.instances.*
    
    // Lift a value into Option
    val lifted: Option[String] = "hello".pure[Option]
    // Result: Some("hello")
    
    // Lift into Either
    val liftedEither: Either[String, Int] = 42.pure[Either[String, *]]
    // Result: Right(42)
    ```

!!! example "Combining contexts with `map2`"

    ```scala
    import hearth.fp.syntax.*
    import hearth.fp.instances.*
    
    // Combine two Options
    val name: Option[String] = Some("Alice")
    val age: Option[Int] = Some(30)
    val person: Option[String] = name.map2(age) { (n, a) =>
      s"$n is $a years old"
    }
    // Result: Some("Alice is 30 years old")
    
    // Combine with failure (fail-fast)
    val name2: Option[String] = Some("Bob")
    val age2: Option[Int] = None
    val person2: Option[String] = name2.map2(age2) { (n, a) =>
      s"$n is $a years old"
    }
    // Result: None (fail-fast semantics)
    ```

### `Parallel`

Extends `Applicative` with parallel semantics. Unlike `Applicative` which has fail-fast behavior, `Parallel` can aggregate errors and run computations independently.

!!! example "Parallel error aggregation"

    ```scala
    import hearth.fp.syntax.*
    import hearth.fp.instances.*
    
    // Using Either[List[String], *] for error aggregation
    val name: Either[List[String], String] = Left(List("Name is required"))
    val age: Either[List[String], Int] = Left(List("Age must be positive"))
    
    val result: Either[List[String], String] = name.parMap2(age) { (n, a) =>
      s"$n is $a years old"
    }
    // Result: Left(List("Name is required", "Age must be positive"))
    // Both errors are collected, not just the first one
    ```

!!! example "Parallel vs Sequential behavior"

    ```scala
    import hearth.fp.syntax.*
    import hearth.fp.instances.*
    
    // Sequential (Applicative) - fails on first error
    val seqResult = name.map2(age) { (n, a) => s"$n is $a" }
    // Result: Left(List("Name is required")) - stops at first error
    
    // Parallel - collects all errors
    val parResult = name.parMap2(age) { (n, a) => s"$n is $a" }
    // Result: Left(List("Name is required", "Age must be positive"))
    ```

### `Traverse`

Represents types that can be traversed with an effect. Allows transforming each element with an effectful function and collecting the results.

!!! example "Traversing collections"

    ```scala
    import hearth.fp.syntax.*
    import hearth.fp.instances.*
    
    // Traverse List with Option
    val numbers: List[Int] = List(1, 2, 3, 4)
    val validateEven: Int => Option[Int] = n => 
      if (n % 2 == 0) Some(n) else None
    
    val result: Option[List[Int]] = numbers.traverse(validateEven)
    // Result: None (because 1 and 3 are not even)
    
    // Traverse with success
    val evenNumbers: List[Int] = List(2, 4, 6, 8)
    val success: Option[List[Int]] = evenNumbers.traverse(validateEven)
    // Result: Some(List(2, 4, 6, 8))
    ```

!!! example "Sequencing effects"

    ```scala
    import hearth.fp.syntax.*
    import hearth.fp.instances.*
    
    // Convert List[Option[A]] to Option[List[A]]
    val maybeNumbers: List[Option[Int]] = List(Some(1), Some(2), None, Some(4))
    val sequenced: Option[List[Int]] = maybeNumbers.sequence
    // Result: None (because of the None in the middle)
    
    // All Some values
    val allSome: List[Option[Int]] = List(Some(1), Some(2), Some(3))
    val sequenced2: Option[List[Int]] = allSome.sequence
    // Result: Some(List(1, 2, 3))
    ```

!!! example "Parallel traversal"

    ```scala
    import hearth.fp.syntax.*
    import hearth.fp.instances.*
    
    // Parallel traversal with error aggregation
    val validateName: String => Either[List[String], String] = name =>
      if (name.isEmpty) Left(List("Name cannot be empty"))
      else Right(name)
    
    val validateAge: Int => Either[List[String], Int] = age =>
      if (age < 0) Left(List("Age must be positive"))
      else Right(age)
    
    val names: List[String] = List("", "Alice", "")
    val ages: List[Int] = List(-1, 30, -5)
    
    val nameResults: Either[List[String], List[String]] = 
      names.traverse(validateName)
    val ageResults: Either[List[String], List[Int]] = 
      ages.traverse(validateAge)
    
    // Combine with parallel semantics
    val combined: Either[List[String], List[String]] = 
      nameResults.parMap2(ageResults) { (ns, as) =>
        ns.zip(as).map { case (n, a) => s"$n is $a" }
      }
    // Result: Left(List("Name cannot be empty", "Age must be positive", "Name cannot be empty", "Age must be positive"))
    ```

### `DirectStyle`

Provides a way to write effectful code in a direct style, similar to using `for` comprehensions but with more flexibility. Useful for complex nested computations where normal combinators are awkward.

!!! example "Direct style with Option"

    ```scala
    import hearth.fp.syntax.*
    import hearth.fp.instances.*
    
    val result: Option[String] = DirectStyle[Option].scoped { runSafe =>
      val name: String = runSafe(Some("Alice"))
      val age: Int = runSafe(Some(30))
      val city: String = runSafe(Some("New York"))
      
      s"$name is $age years old and lives in $city"
    }
    // Result: Some("Alice is 30 years old and lives in New York")
    ```

!!! example "Direct style with error handling"

    ```scala
    import hearth.fp.syntax.*
    import hearth.fp.instances.*
    
    val result: Either[List[String], String] = DirectStyle[Either[List[String], *]].scoped { runSafe =>
      val name: String = runSafe(Right("Bob"))
      val age: Int = runSafe(Left(List("Age is required")))
      val city: String = runSafe(Right("London"))
      
      // This line is never reached due to the error above
      s"$name is $age years old and lives in $city"
    }
    // Result: Left(List("Age is required"))
    ```

!!! example "Complex macro generation"

    ```scala
    import hearth.fp.syntax.*
    import hearth.fp.instances.*
    
    // Example of generating complex macro code
    def generateTypeClass[A]: MIO[Expr[TypeClass[A]]] = DirectStyle[MIO].scoped { runSafe =>
      '{
        new TypeClass[A] {
          def method(arg: A): String = ${
            // Extract Expr[String] from MIO[Expr[String]]
            runSafe(generateMethodImpl('{ arg }))
          }
          
          def anotherMethod(arg: A): Int = ${
            runSafe(generateAnotherImpl('{ arg }))
          }
        }
      }
    }
    ```

### Combined Type Classes

The library provides combined type classes to avoid implicit resolution conflicts:

#### `ApplicativeTraverse`

Combines `Applicative[F]` and `Traverse[F]`. Useful when you need both capabilities.

#### `ParallelTraverse`

Combines `Parallel[F]` and `Traverse[F]`. Provides both parallel semantics and traversal capabilities.

### Creating Type Class Instances

To implement type classes for your own types, follow these patterns:

!!! example "Implementing Functor"

    ```scala
    import hearth.fp.*
    
    // For a custom container type
    case class Box[A](value: A)
    
    implicit val FunctorForBox: Functor[Box] = new Functor[Box] {
      def map[A, B](fa: Box[A])(f: A => B): Box[B] = 
        Box(f(fa.value))
    }
    
    // Usage
    val box: Box[Int] = Box(42)
    val doubled: Box[Int] = box.map(_ * 2)
    // Result: Box(84)
    ```

!!! example "Implementing Applicative"

    ```scala
    import hearth.fp.*
    
    implicit val ApplicativeForBox: Applicative[Box] = new Applicative[Box] {
      def pure[A](a: A): Box[A] = Box(a)
      
      def map2[A, B, C](fa: Box[A], fb: => Box[B])(f: (A, B) => C): Box[C] = 
        Box(f(fa.value, fb.value))
    }
    
    // Usage
    val box1: Box[Int] = Box(10)
    val box2: Box[Int] = Box(20)
    val sum: Box[Int] = box1.map2(box2)(_ + _)
    // Result: Box(30)
    ```

!!! example "Implementing Traverse"

    ```scala
    import hearth.fp.*
    
    // For a custom tree structure
    sealed trait Tree[A]
    case class Leaf[A](value: A) extends Tree[A]
    case class Node[A](left: Tree[A], right: Tree[A]) extends Tree[A]
    
    implicit val TraverseForTree: Traverse[Tree] = new Traverse[Tree] {
      def traverse[G[_]: Applicative, A, B](fa: Tree[A])(f: A => G[B]): G[Tree[B]] = 
        fa match {
          case Leaf(a) => f(a).map(Leaf(_))
          case Node(left, right) => 
            left.traverse(f).map2(right.traverse(f))(Node(_, _))
        }
      
      def parTraverse[G[_]: Parallel, A, B](fa: Tree[A])(f: A => G[B]): G[Tree[B]] = 
        fa match {
          case Leaf(a) => f(a).map(Leaf(_))
          case Node(left, right) => 
            left.parTraverse(f).parMap2(right.parTraverse(f))(Node(_, _))
        }
    }
    ```

### Quick Reference

| Type Class       | Purpose                  | Key Methods            | Import               |
|------------------|--------------------------|------------------------|----------------------|
| `Functor[F]`     | Map over values          | `map`                  | `hearth.fp.syntax.*` |
| `Applicative[F]` | Lift and combine         | `pure`, `map2`         | `hearth.fp.syntax.*` |
| `Parallel[F]`    | Parallel combination     | `parMap2`              | `hearth.fp.syntax.*` |
| `Traverse[F]`    | Traverse with effects    | `traverse`, `sequence` | `hearth.fp.syntax.*` |
| `DirectStyle[F]` | Direct style programming | `scoped`               | `hearth.fp.syntax.*` |

### Built-in Instances

The library provides instances for common Scala types:

- **`Id`**: Identity type (no wrapper)
- **`Option`**: Optional values
- **`Either[E, *]`**: Error handling (single error)
- **`Either[List[E], *]`**: Error aggregation
- **`Either[Vector[E], *]`**: Error aggregation
- **`Try`**: Exception handling
- **`List`**: Sequential collections
- **`Vector`**: Indexed collections
- **`NonEmptyList`**: Non-empty lists
- **`NonEmptyVector`**: Non-empty vectors

## Data

The library provides a few essential data types that are commonly needed in functional programming.

### `NonEmptyList`

A non-empty list that guarantees at least one element. Useful for representing collections that cannot be empty, such as error messages or required parameters.

!!! example "Creating `NonEmptyList`"

    ```scala
    import hearth.fp.data.*
    
    // Create with constructor
    val nel1: NonEmptyList[Int] = NonEmptyList(1, List(2, 3, 4))
    
    // Create with apply method
    val nel2: NonEmptyList[String] = NonEmptyList("hello", "world", "!")
    
    // Create single element
    val single: NonEmptyList[Int] = NonEmptyList.one(42)
    
    // Convert from List (returns Option)
    val fromList: Option[NonEmptyList[Int]] = NonEmptyList.fromList(List(1, 2, 3))
    val fromEmpty: Option[NonEmptyList[Int]] = NonEmptyList.fromList(List.empty)
    // Result: Some(NonEmptyList(1, List(2, 3))) and None
    ```

!!! example "Operations on `NonEmptyList`"

    ```scala
    import hearth.fp.data.*
    import hearth.fp.syntax.*
    import hearth.fp.instances.*
    
    val nel: NonEmptyList[Int] = NonEmptyList(1, List(2, 3))
    
    // Prepend element
    val prepended: NonEmptyList[Int] = 0 +: nel
    // Result: NonEmptyList(0, List(1, 2, 3))
    
    // Append element
    val appended: NonEmptyList[Int] = nel :+ 4
    // Result: NonEmptyList(1, List(2, 3, 4))
    
    // Concatenate
    val other: NonEmptyList[Int] = NonEmptyList(5, List(6))
    val combined: NonEmptyList[Int] = nel ++ other
    // Result: NonEmptyList(1, List(2, 3, 5, 6))
    
    // Map over elements
    val doubled: NonEmptyList[Int] = nel.map(_ * 2)
    // Result: NonEmptyList(2, List(4, 6))
    
    // Convert to other collections
    val asList: List[Int] = nel.toList
    val asVector: Vector[Int] = nel.toVector
    val asNEV: NonEmptyVector[Int] = nel.toNonEmptyVector
    ```

!!! example "Error aggregation with `NonEmptyList`"

    ```scala
    import hearth.fp.data.*
    import hearth.fp.syntax.*
    import hearth.fp.instances.*
    
    // Validate multiple fields and collect all errors
    def validateName(name: String): Either[NonEmptyList[String], String] =
      if (name.isEmpty) Left(NonEmptyList.one("Name cannot be empty"))
      else if (name.length < 2) Left(NonEmptyList.one("Name must be at least 2 characters"))
      else Right(name)
    
    def validateAge(age: Int): Either[NonEmptyList[String], Int] =
      if (age < 0) Left(NonEmptyList.one("Age must be positive"))
      else if (age > 150) Left(NonEmptyList.one("Age must be reasonable"))
      else Right(age)
    
    // Parallel validation aggregates all errors
    val name: Either[NonEmptyList[String], String] = Left(NonEmptyList("Name cannot be empty", "Name too short"))
    val age: Either[NonEmptyList[String], Int] = Left(NonEmptyList.one("Age must be positive"))
    
    val result: Either[NonEmptyList[String], String] = name.parMap2(age) { (n, a) =>
      s"$n is $a years old"
    }
    // Result: Left(NonEmptyList("Name cannot be empty", "Name too short", "Age must be positive"))
    ```

### `NonEmptyVector`

A non-empty vector that provides indexed access and better performance for larger collections compared to `NonEmptyList`.

!!! example "Creating `NonEmptyVector`"

    ```scala
    import hearth.fp.data.*
    
    // Create with constructor
    val nev1: NonEmptyVector[Int] = NonEmptyVector(1, Vector(2, 3, 4))
    
    // Create with apply method
    val nev2: NonEmptyVector[String] = NonEmptyVector("hello", "world", "!")
    
    // Create single element
    val single: NonEmptyVector[Int] = NonEmptyVector.one(42)
    
    // Convert from Vector (returns Option)
    val fromVector: Option[NonEmptyVector[Int]] = NonEmptyVector.fromVector(Vector(1, 2, 3))
    val fromEmpty: Option[NonEmptyVector[Int]] = NonEmptyVector.fromVector(Vector.empty)
    // Result: Some(NonEmptyVector(1, Vector(2, 3))) and None
    ```

!!! example "Operations on `NonEmptyVector`"

    ```scala
    import hearth.fp.data.*
    import hearth.fp.syntax.*
    import hearth.fp.instances.*
    
    val nev: NonEmptyVector[Int] = NonEmptyVector(1, Vector(2, 3))
    
    // Prepend element
    val prepended: NonEmptyVector[Int] = 0 +: nev
    // Result: NonEmptyVector(0, Vector(1, 2, 3))
    
    // Append element
    val appended: NonEmptyVector[Int] = nev :+ 4
    // Result: NonEmptyVector(1, Vector(2, 3, 4))
    
    // Concatenate
    val other: NonEmptyVector[Int] = NonEmptyVector(5, Vector(6))
    val combined: NonEmptyVector[Int] = nev ++ other
    // Result: NonEmptyVector(1, Vector(2, 3, 5, 6))
    
    // Map over elements
    val doubled: NonEmptyVector[Int] = nev.map(_ * 2)
    // Result: NonEmptyVector(2, Vector(4, 6))
    
    // Convert to other collections
    val asList: List[Int] = nev.toList
    val asVector: Vector[Int] = nev.toVector
    val asNEL: NonEmptyList[Int] = nev.toNonEmptyList
    ```

!!! example "Error aggregation with `NonEmptyVector`"

    ```scala
    import hearth.fp.data.*
    import hearth.fp.syntax.*
    import hearth.fp.instances.*
    
    // Validate multiple fields and collect all errors
    def validateEmail(email: String): Either[NonEmptyVector[String], String] =
      if (email.isEmpty) Left(NonEmptyVector.one("Email cannot be empty"))
      else if (!email.contains("@")) Left(NonEmptyVector.one("Email must contain @"))
      else Right(email)
    
    def validatePhone(phone: String): Either[NonEmptyVector[String], String] =
      if (phone.isEmpty) Left(NonEmptyVector.one("Phone cannot be empty"))
      else if (phone.length < 10) Left(NonEmptyVector.one("Phone must be at least 10 digits"))
      else Right(phone)
    
    // Parallel validation aggregates all errors
    val email: Either[NonEmptyVector[String], String] = Left(NonEmptyVector("Email cannot be empty", "Email must contain @"))
    val phone: Either[NonEmptyVector[String], String] = Left(NonEmptyVector.one("Phone cannot be empty"))
    
    val result: Either[NonEmptyVector[String], String] = email.parMap2(phone) { (e, p) =>
      s"Contact: $e, $p"
    }
    // Result: Left(NonEmptyVector("Email cannot be empty", "Email must contain @", "Phone cannot be empty"))
    ```

### `NonEmptyMap`

A non-empty `ListMap` that guarantees at least one key-value pair and preserves the order of insertion.

!!! example "Creating `NonEmptyMap`"

    ```scala
    import hearth.fp.data.*
    
    // Create with constructor
    val nem1: NonEmptyMap[String, Int] = NonEmptyMap(("a", 1), ListMap("b" -> 2, "c" -> 3))
    
    // Create with apply method
    val nem2: NonEmptyMap[String, String] = NonEmptyMap(("name", "Alice"), ("age", "30"), ("city", "New York"))
    
    // Create single element
    val single: NonEmptyMap[String, Int] = NonEmptyMap.one(("key", 42))
    
    // Convert from ListMap (returns Option)
    val fromListMap: Option[NonEmptyMap[String, Int]] = NonEmptyMap.fromListMap(ListMap("a" -> 1, "b" -> 2, "c" -> 3))
    val fromEmpty: Option[NonEmptyMap[String, Int]] = NonEmptyMap.fromListMap(ListMap.empty)
    // Result: Some(NonEmptyMap(("a", 1), ListMap("b" -> 2, "c" -> 3))) and None
    ```

!!! example "Operations on `NonEmptyMap`"

    ```scala
    import hearth.fp.data.*
    import hearth.fp.syntax.*
    import hearth.fp.instances.*
    import scala.collection.immutable.ListMap
    
    val nem: NonEmptyMap[String, Int] = NonEmptyMap(("a", 1), ListMap("b" -> 2, "c" -> 3))
    
    // Prepend element
    val prepended: NonEmptyMap[String, Int] = ("x", 0) +: nem
    // Result: NonEmptyMap(("x", 0), ListMap("a" -> 1, "b" -> 2, "c" -> 3))
    
    // Append element
    val appended: NonEmptyMap[String, Int] = nem :+ ("d", 4)
    // Result: NonEmptyMap(("a", 1), ListMap("b" -> 2, "c" -> 3, "d" -> 4))
    
    // Map over key-value pairs
    val mapped: NonEmptyMap[String, String] = nem.map { case (k, v) => (k.toUpperCase, v.toString) }
    // Result: NonEmptyMap(("A", "1"), ListMap("B" -> "2", "C" -> "3"))
    
    // FlatMap over key-value pairs
    val flatMapped: NonEmptyMap[String, Int] = nem.flatMap { case (k, v) =>
      NonEmptyMap.one((k + "_copy", v * 2))
    }
    // Result: NonEmptyMap(("a_copy", 2), ListMap("b_copy" -> 4, "c_copy" -> 6))
    
    // Convert to other collections
    val asListMap: ListMap[String, Int] = nem.toListMap
    val asList: List[(String, Int)] = nem.toList
    val asVector: Vector[(String, Int)] = nem.toVector
    val asNEL: NonEmptyList[(String, Int)] = nem.toNonEmptyList
    val asNEV: NonEmptyVector[(String, Int)] = nem.toNonEmptyVector
    ```

## Macro IO (`MIO`)

`MIO` (Macro IO) is a specialized effect type designed for safe data transformations in macros. It provides stack-safety, structured logging, error aggregation, and referential transparency without external dependencies.

### Key Features

- **Stack-safe**: Handles deep-nested computations without stack overflow
- **Structured logging**: Build logs without macro reporter limitations
- **Error aggregation**: Collect multiple errors in `NonEmptyVector[Throwable]`
- **Exception safety**: Catches non-fatal errors automatically
- **Referential transparency**: Values can be safely reused

### Basic Usage

!!! example "Simple `MIO` operations"

    ```scala
    import hearth.fp.effect.*
    import hearth.fp.syntax.*
    import hearth.fp.instances.*
    
    // Pure value
    val pureValue: MIO[Int] = MIO.pure(42)
    
    // Suspend computation
    val suspended: MIO[String] = MIO.suspend(Right("hello"))
    
    // Map over result
    val mapped: MIO[Int] = pureValue.map(_ * 2)
    
    // Combine with map2
    val combined: MIO[String] = pureValue.map2(suspended) { (i, s) =>
      s"$s: $i"
    }
    // Result: "hello: 42"
    ```

!!! example "Error handling"

    ```scala
    import hearth.fp.effect.*
    import hearth.fp.syntax.*
    import hearth.fp.instances.*
    
    // Suspend with error
    val error: MIO[Int] = MIO.suspend(Left(NonEmptyVector.one(new RuntimeException("Something went wrong"))))
    
    // Error aggregation
    val error1: MIO[Int] = MIO.suspend(Left(NonEmptyVector.one(new RuntimeException("Error 1"))))
    val error2: MIO[Int] = MIO.suspend(Left(NonEmptyVector.one(new RuntimeException("Error 2"))))
    
    val aggregated: MIO[Int] = error1.parMap2(error2)(_ + _)
    // Both errors are collected in the result
    ```

### Logging

!!! example "Structured logging"

    ```scala
    import hearth.fp.effect.*
    import hearth.fp.syntax.*
    import hearth.fp.instances.*
    
    // Log messages
    val logged: MIO[Unit] = for {
      _ <- Log.info("Starting computation")
      _ <- Log.warn("This is a warning")
      _ <- Log.error("This is an error")
    } yield ()
    
    // Named scopes for better organization
    val scoped: MIO[Int] = Log.namedScope("validation") {
      for {
        _ <- Log.info("Validating input")
        result <- MIO.pure(42)
        _ <- Log.info("Validation complete")
      } yield result
    }
    ```

Within `MacroCommons` there is also an integration provided, that would extract successful value,
log and fail if necessary:

!!! example "Convert errors to error message, log using `info`"

    ```scala
    def deriveOrFail[A: Type](value: Expr[A], name: String): Expr[String] = Log
      .namedScope(s"Derivation for $name") {
        attemptAllRules[A](value)
      }
      .runToExprOrFail(
        name,
        // Renders all logs if `shouldWeLogDerivation` and none otherwise
        infoRendering = if (shouldWeLogDerivation) RenderFrom(Log.Level.Info) else DontRender
      ) { (errorLogs, errors) =>
        // errorLogs: String - pretty-printed log
        // errors: NonEmptyVector[Throwable] - errors that happened during the derivation (if it succeeded, this wouldn't be called)

        val errorsStr = errors.toVector
          .map {
            case DerivationError.UnsupportedType(typeName)           => s"Derivation of $typeName is not supported"
            case DerivationError.UnsupportedMethod(typeName, method) =>
              s"Derivation of $typeName.$method is not supported"
            case DerivationError.AssertionFailed(message) => s"Assertion failed: $message"
            case e => s"Unexpected error: ${e.getMessage}:\n${e.getStackTrace.mkString("\n")}"
          }
          .mkString("\n")

        if (errorLogs.nonEmpty) {
          s"""Failed to derive $name:
            |$errorsStr
            |Error logs:
            |$errorLogs
            |""".stripMargin
        } else {
          s"""Failed to derive $name:
            |$errorsStr
            |""".stripMargin
        }
      }
    ```

### Running `MIO`

!!! example "Running `MIO` computations"

    ```scala
    import hearth.fp.effect.*
    import hearth.fp.syntax.*
    import hearth.fp.instances.*
    
    val computation: MIO[String] = for {
      _ <- Log.info("Starting")
      value <- MIO.pure("hello")
      _ <- Log.info("Computation complete")
    } yield value
    
    // Run and get result with logs
    val (state, result) = computation.unsafe.runSync
    
    // Render logs
    val logOutput: String = state.logs.render.fromInfo("MyApp")
    println(logOutput)
    // Output:
    // MyApp:
    // ├─ [Info]  Starting
    // └─ [Info]  Computation complete
    ```

### Error-Handling Patterns

!!! example "Safe computation with error recovery"

    ```scala
    import hearth.fp.effect.*
    import hearth.fp.syntax.*
    import hearth.fp.instances.*
    
    def safeDivide(a: Int, b: Int): MIO[Int] = MIO(a / b)
    
    // Multiple computations with error aggregation
    val computations: MIO[List[Int]] = List(
      safeDivide(10, 2),
      safeDivide(10, 0),  // This will fail
      safeDivide(10, 5)
    ).parSequence
    
    val (_, result) = computations.unsafe.runSync
    ```

### Integration with Type Classes

`MIO` implements all the type classes discussed earlier, making it easy to integrate with existing functional programming patterns:

!!! example "`MIO` with type classes"

    ```scala
    import hearth.fp.effect.*
    import hearth.fp.syntax.*
    import hearth.fp.instances.*
    
    // Usage with traverse
    val items: List[Int] = List(1, 2, 3, 4)
    val processed: MIO[List[String]] = items.traverse { item =>
      for {
        _ <- Log.info(s"Processing item $item")
        result <- MIO.pure(s"Processed: $item")
      } yield result
    }
    
    // Usage with direct style
    val directResult: MIO[String] = DirectStyle[MIO].scoped { runSafe =>
      val a: Int = runSafe(MIO.pure(10))
      val b: Int = runSafe(MIO.pure(20))
      s"Sum: ${a + b}"
    }
    ```
