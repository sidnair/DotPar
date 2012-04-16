/* Using scala to write out a full compilation input/output test suite
 * Just use it as a scala script
 */

import java.io.File
import scala.sys.process._
import scala.sys.process.ProcessIO

val DEBUG = true

def list_files(dir_path: String): Array[File] = {
  val f = new File(dir_path)
  val these = f.listFiles
  these.filter(f => """.*\.par$""".r.findFirstIn(f.getName).isDefined)
}

val cats = List("basic", "arrays", "examples")

// returns success ("") or error message
def execute_test(f : File): String = {
  // get the associated .out output
  val out_path = f.getAbsolutePath().split('.').init :+ "out" mkString "."
  val expected_output = scala.io.Source.fromFile(out_path).mkString
  // run the compiler
  var output:List[String] = List()
  var errors:List[String] = List()
  def update_output(str: String) = {
    output = output :+ str
  }
  def update_errors(str: String) = {
    errors = errors :+ str
  }
  val proc = Process("""scripts/dotparc.sh """ + f.getAbsolutePath())
  val pio = new ProcessIO(_ => (),
                          stdout => scala.io.Source.fromInputStream(stdout)
                          .getLines().foreach(update_output(_)),
                          stderr => scala.io.Source.fromInputStream(stderr)
                          .getLines().foreach(update_errors(_)))
  val exitCode = proc.!
  print(output)
  print(errors)
  // !!!
  print("\t" + f.getName() + "\n")
  ""
}

def execute_tests(dir: String) = {
  if(DEBUG)
    print("testing directory: " + dir + "\n")
  val files = list_files("tests/" + dir)
  // for each file, run the compiler on it, run the jar, and check output
  val statuses = files.map(execute_test)
  // filter out the successes
  val errors = statuses.filter(_.length > 0)
  // display the errors
  errors.map(print(_))
}

// excecute all the tests across the categories
cats.map(execute_tests)
