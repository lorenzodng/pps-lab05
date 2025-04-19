package ex

import util.Optionals.Optional
import util.Sequences.*

import scala.collection.mutable
import scala.collection.mutable.ListBuffer // Assuming Sequence and related methods are here

// Represents a course offered on the platform
trait Course:
  def courseId: String // Unique identifier (e.g., "CS101", "SCALA01")
  def title: String
  def instructor: String
  def category: String // e.g., "Programming", "Data Science", "Design"

object Course:
  // Factory method for creating Course instances
  def apply(courseId: String, title: String, instructor: String, category: String): Course = CourseImpl(courseId, title, instructor, category)

  case class CourseImpl(courseId: String, title: String, instructor: String, category: String) extends Course
/**
 * Manages courses and student enrollments on an online learning platform.
 */
trait OnlineCoursePlatform:
  /**
   * Adds a new course to the platform's catalog.
   * @param course The course to add.
   */
  def addCourse(course: Course): Unit

  /**
   * Finds courses belonging to a specific category.
   * @param category The category to search for.
   * @return A sequence of courses in that category.
   */
  def findCoursesByCategory(category: String): Sequence[Course]

  /**
   * Retrieves a specific course by its unique ID.
   * @param courseId The ID of the course to retrieve.
   * @return An Optional containing the course if found, otherwise Optional.empty.
   */
  def getCourse(courseId: String): Optional[Course]

  /**
   * Removes a course from the platform's catalog.
   * (Note: This basic version doesn't handle cascading removal of enrollments).
   * @param course The course to remove.
   */
  def removeCourse(course: Course): Unit

  /**
   * Checks if a course with the given ID exists in the catalog.
   * @param courseId The ID to check.
   * @return true if the course exists, false otherwise.
   */
  def isCourseAvailable(courseId: String): Boolean

  /**
   * Enrolls a student in a specific course.
   * Assumes studentId is unique for each student.
   * @param studentId The ID of the student.
   * @param courseId The ID of the course to enroll in.
   *                 Fails silently if the course doesn't exist.
   */
  def enrollStudent(studentId: String, courseId: String): Unit

  /**
   * Unenrolls a student from a specific course.
   * @param studentId The ID of the student.
   * @param courseId The ID of the course to unenroll from.
   */
  def unenrollStudent(studentId: String, courseId: String): Unit

  /**
   * Retrieves all courses a specific student is enrolled in.
   * @param studentId The ID of the student.
   * @return A sequence of courses the student is enrolled in.
   */
  def getStudentEnrollments(studentId: String): Sequence[Course]

  /**
   * Checks if a student is enrolled in a specific course.
   * @param studentId The ID of the student.
   * @param courseId The ID of the course.
   * @return true if the student is enrolled, false otherwise.
   */
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

/**
 * Represents an online learning platform that offers courses and manages student enrollments.
 * Hints:
 * - Start by implementing the Course trait.
 *    - A case class might be a good fit for this.
 * - Implement the OnlineCoursePlatform trait.
 *    - Focus on how to represent the internal state
 *    - Two main entities: courses and student enrollments
 *    - Set for courses? List of enrollments?
 *  - Implement the factory method for creating an empty platform instance.
 *  - Now start incrementally following the main given
 *
 */
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
