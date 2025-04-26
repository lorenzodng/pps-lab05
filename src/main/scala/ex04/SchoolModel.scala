package ex04

import util.Sequences.Sequence
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

trait Teacher:
  def name: String

end Teacher

object Teacher:
  def apply(name: String): Teacher = TeacherImpl(name)

  case class TeacherImpl(name: String) extends Teacher

end Teacher

trait Course:
  def name: String

end Course

object Course:
  def apply(name: String): Course = CourseImpl(name)

  case class CourseImpl(name: String) extends Course

end Course

trait SchoolModule:

  def courses: Sequence[String]

  def teachers: Sequence[String]

  def setTeacherToCourse(teacher: Teacher, course: Course): SchoolModule

  def coursesOfATeacher(teacher: Teacher): Sequence[Course]

  def hasTeacher(name: String): Boolean

  def hasCourse(name: String): Boolean

end SchoolModule

object SchoolModule:
  def apply(courses: mutable.Set[Course], teachers: mutable.Set[Teacher], teachers_courses: ListBuffer[(Teacher, Course)]): SchoolModule = SchoolModuleImpl(courses, teachers, teachers_courses)

  case class SchoolModuleImpl(coursesSchool: mutable.Set[Course], teachersSchool: mutable.Set[Teacher], teachersCourses: ListBuffer[(Teacher, Course)]) extends SchoolModule:

    def courses: Sequence[String] =
      var coursesName: Sequence[String] = Sequence.Nil()
      for course <- coursesSchool do
        coursesName = Sequence.Cons(course.name, coursesName)

      coursesName

    def teachers: Sequence[String] =
      var teachersName: Sequence[String] = Sequence.Nil()
      for teacher <- teachersSchool do
        teachersName = Sequence.Cons(teacher.name, teachersName)

      teachersName

    def setTeacherToCourse(teacher: Teacher, course: Course): SchoolModuleImpl = SchoolModuleImpl(coursesSchool.addOne(course), teachersSchool.addOne(teacher), teachersCourses.addOne(teacher, course))

    def coursesOfATeacher(teacher: Teacher): Sequence[Course] =
      val filteredTeachersCourses = teachersCourses.filter(teacherCourse => teacherCourse._1 == teacher)

      var courses: Sequence[Course] = Sequence.Nil()
      for teacherCourse <- filteredTeachersCourses do
        courses = Sequence.Cons(teacherCourse._2, courses)

      courses

    def hasTeacher(name: String): Boolean =
      var check = false
      for teacher <- teachersSchool do
          if teacher.name.equals(name) then
            check = true

      check

    def hasCourse(name: String): Boolean =
      var check = false
      for course <- coursesSchool do
          if course.name.equals(name) then
            check = true

      check

end SchoolModule

@main def examples(): Unit =

  import ex04.SchoolModule.SchoolModuleImpl

  val school = SchoolModuleImpl(mutable.Set.empty[Course], mutable.Set.empty[Teacher], ListBuffer.empty[(Teacher, Course)])

  val john = Teacher("John")
  val math = Course("Math")
  val italian = Course("Italian")
  val school2 = school.setTeacherToCourse(john, math)
  println(school2.teachers) // Cons("John", Nil())
  println(school2.courses) // Cons("Math", Nil())
  println(school2.hasTeacher("John")) // true
  println(school2.hasCourse("Math")) // true
  println(school2.hasCourse("Italian")) // false
  val school3 = school2.setTeacherToCourse(john, italian)
  println(school3.courses) // Cons("Math", Cons("Italian", Nil()))
  println(school3.hasTeacher("John")) // true
  println(school3.hasCourse("Math")) // true
  println(school3.hasCourse("Italian")) // true
  println(school3.coursesOfATeacher(john)) // Cons("Math", Cons("Italian", Nil()))*/
