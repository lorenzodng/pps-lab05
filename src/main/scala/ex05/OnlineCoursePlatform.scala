package ex05

import util.Sequences.Sequence
import scala.annotation.tailrec

//Exercise 5
trait Course:
  def courseId: String
  def title: String
  def instructor: String
  def category: String

object Course:

  def apply(courseId: String, title: String, instructor: String, category: String): Course = CourseImpl(courseId, title, instructor, category)

  case class CourseImpl(courseId: String, title: String, instructor: String, category: String) extends Course

end Course

object sameCategory:
  def unapply(courses: Sequence[Course]): Option[String] = courses match
    case Sequence.Cons(head, tail) => checkCategory(courses, head.category)
    case _ => None

  @tailrec
  private def checkCategory(courses: Sequence[Course], category: String): Option[String] = courses match
    case Sequence.Cons(head, tail) if head.category.equals(category) => checkCategory(tail, head.category)
    case Sequence.Nil() => Some(category)
    case _ => None


@main def mainPlatform(): Unit =

  val scalaCourse = Course("SCALA01", "Functional Programming in Scala", "Prof. Odersky", "Programming")
  val pythonCourse = Course("PYTHON01", "Introduction to Python", "Prof. van Rossum", "Programming")

  val courses = Sequence(scalaCourse, pythonCourse, pythonCourse)
  courses match
    case sameCategory(cat) => println(s"$courses have same category $cat")
    case _ => println(s"$courses have different categories")
