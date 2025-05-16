package ex

import util.Optionals.Optional
import util.Sequences.*

import scala.collection.mutable
import scala.collection.mutable.ListBuffer 

//Exercise 1
trait Course:
  def courseId: String 
  def title: String
  def instructor: String
  def category: String

object Course:
  def apply(courseId: String, title: String, instructor: String, category: String): Course = CourseImpl(courseId, title, instructor, category)

  case class CourseImpl(courseId: String, title: String, instructor: String, category: String) extends Course

trait OnlineCoursePlatform:
 
  def addCourse(course: Course): Unit

  def findCoursesByCategory(category: String): Sequence[Course]
  
  def getCourse(courseId: String): Optional[Course]

  def removeCourse(course: Course): Unit

  def isCourseAvailable(courseId: String): Boolean

  def enrollStudent(studentId: String, courseId: String): Unit

  def unenrollStudent(studentId: String, courseId: String): Unit
 
  def getStudentEnrollments(studentId: String): Sequence[Course]

  def isStudentEnrolled(studentId: String, courseId: String): Boolean

end OnlineCoursePlatform

object OnlineCoursePlatform:

  def apply(courses: mutable.Set[Course], students: List[String], students_courses: ListBuffer[(String, String)]): OnlineCoursePlatform = OnlineCoursePlatformImpl(courses, students, students_courses)

  case class OnlineCoursePlatformImpl(courses: mutable.Set[Course], students: List[String], studentsCourses: ListBuffer[(String, String)]) extends OnlineCoursePlatform:

    def addCourse(course: Course): Unit = courses.add(course)

    def findCoursesByCategory(category: String): Sequence[Course] =
      val filteredCourses = courses.filter(course => course.category == category)

      var sequence: Sequence[Course] = Sequence.Nil()
      for course <- filteredCourses do
        sequence = Sequence.Cons(course, sequence)

      sequence

    def getCourse(courseId: String): Optional[Course] =
      val filteredCourses = courses.filter(course => course.courseId == courseId)

      var optional: Optional[Course] = Optional.Empty()
      for course <- filteredCourses do
        optional= Optional.Just(course)

      optional

    def removeCourse(course: Course): Unit = courses.remove(course)

    def isCourseAvailable(courseId: String): Boolean =
      val filteredCourses = courses.filter(course => course.courseId == courseId)

      if filteredCourses.nonEmpty
      then
        true
      else
        false

    def enrollStudent(studentId: String, courseId: String): Unit = studentsCourses.addOne(studentId, courseId)

    def unenrollStudent(studentId: String, courseId: String): Unit =
      val students_courses2 = studentsCourses.toList
      for i <- 0 to (students_courses2.size - 1)
        if students_courses2(i)._1.equals(studentId) && students_courses2(i)._2.equals(courseId)
        do
          studentsCourses.remove(i)

    def getStudentEnrollments(studentId: String): Sequence[Course] =
      val filteredStudentsCourses = studentsCourses.filter(student => student._1.equals(studentId)).toList

      var sequence: Sequence[Course] = Sequence.Nil()
      for student_course <- filteredStudentsCourses do
        val filteredCourses = courses.filter(course => student_course._2.equals(course.courseId))
        sequence = Sequence.Cons(filteredCourses.head, sequence)

      sequence

    def isStudentEnrolled(studentId: String, courseId: String): Boolean =
      var check = false
      for student_course <- studentsCourses
        if studentsCourses.contains(student_course._1.equals(studentId) && student_course._2.equals(courseId)) do
        check = true

      if check then
        true
      else
        false

@main def mainPlatform(): Unit =
  import ex.OnlineCoursePlatform.OnlineCoursePlatformImpl

  val scalaCourse = Course("SCALA01", "Functional Programming in Scala", "Prof. Odersky", "Programming")
  val pythonCourse = Course("PYTHON01", "Introduction to Python", "Prof. van Rossum", "Programming")
  val designCourse = Course("DESIGN01", "UI/UX Design Fundamentals", "Prof. Norman", "Design")

  val platform = OnlineCoursePlatformImpl(mutable.Set.empty[Course], List.empty[String], ListBuffer.empty[(String, String)])

  println(s"Is SCALA01 available? ${platform.isCourseAvailable(scalaCourse.courseId)}") // false
  platform.addCourse(scalaCourse)
  println(s"Is SCALA01 available? ${platform.isCourseAvailable(scalaCourse.courseId)}") // true
  platform.addCourse(pythonCourse)
  platform.addCourse(designCourse)

  println(s"Programming courses: ${platform.findCoursesByCategory("Programming")}") // Sequence(scalaCourse, pythonCourse)
  println(s"Design courses: ${platform.findCoursesByCategory("Design")}") // Sequence(designCourse)
  println(s"History courses: ${platform.findCoursesByCategory("History")}") // Sequence.empty

  println(s"Get SCALA01: ${platform.getCourse("SCALA01")}") // Optional.Just(scalaCourse)
  println(s"Get UNKNOWN01: ${platform.getCourse("UNKNOWN01")}") // Optional.Empty

  // Enrollments
  val studentAlice = "Alice123"
  val studentBob = "Bob456"

  println(s"Is Alice enrolled in SCALA01? ${platform.isStudentEnrolled(studentAlice, "SCALA01")}") // false
  platform.enrollStudent(studentAlice, "SCALA01")
  println(s"Is Alice enrolled in SCALA01? ${platform.isStudentEnrolled(studentAlice, "SCALA01")}") // true
  platform.enrollStudent(studentAlice, "DESIGN01")
  platform.enrollStudent(studentBob, "SCALA01") // Bob also enrolls in Scala

  println(s"Alice's enrollments: ${platform.getStudentEnrollments(studentAlice)}") // Sequence(scalaCourse, designCourse) - Order might vary
  println(s"Bob's enrollments: ${platform.getStudentEnrollments(studentBob)}") // Sequence(scalaCourse)

  platform.unenrollStudent(studentAlice, "SCALA01")
  println(s"Is Alice enrolled in SCALA01? ${platform.isStudentEnrolled(studentAlice, "SCALA01")}") // false
  println(s"Alice's enrollments: ${platform.getStudentEnrollments(studentAlice)}") // Sequence(designCourse)

  // Removal
  platform.removeCourse(pythonCourse)
  println(s"Is PYTHON01 available? ${platform.isCourseAvailable(pythonCourse.courseId)}") // false
  println(s"Programming courses: ${platform.findCoursesByCategory("Programming")}") // Sequence(scalaCourse)
