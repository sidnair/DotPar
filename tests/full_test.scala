/* Using scala to write out a full compilation input/output test suite
 * Just use it as a scala script
 */

import java.io.File
import scala.sys.process._

val DEBUG = true

def list_files(dir_path: String): Array[File] = {
  val f = new File(dir_path)
  val these = f.listFiles
  these.filter(f => """.*\.par$""".r.findFirstIn(f.getName).isDefined)
}

val cats = List("basic", "arrays", "examples")

// returns success ("", false, 0, "")
// or failure (name, compilation_passed, exitcode,out)
def execute_test(f : File):(String, Boolean, Int, String) = {
  // get the associated .out output
  val out_path = f.getAbsolutePath().split('.').init :+ "out" mkString "."
  val expected_output = scala.io.Source.fromFile(out_path).mkString
  // run the compiler
  var errors:List[String] = List()
  val compile = Seq("scripts/dotparc.sh", f.getAbsolutePath())
  val compile_log = ProcessLogger(line => errors = errors :+ line,
                                  line => errors = errors :+ line)
  val exit_compile = compile.!(compile_log)
  // we will always return non-zero exitcodes on failure
  if (exit_compile != 0) {
    print("F")
    val error_str = if (errors.length > 0) errors.reduceLeft(_+"\n"+_) else ""
    return (f.getName(), false, exit_compile, error_str)
  }
  // run the program
  var output:List[String] = List()
  val gen_prog = new File("program.jar")
  val test = "java -jar " + gen_prog.getAbsolutePath()
  val test_log = ProcessLogger(line => output = output :+ line,
                               line => output = output :+ line)
  val exit_test = test.!(test_log)
  // test if output is exactly the same
  val actual_output = if(output.length > 0) output.reduceLeft(_+"\n"+_) else ""
  if (actual_output == expected_output) {
    print(".")
    gen_prog.delete()
    return ("", true, 0, "")
  } else {
    print("F")
    gen_prog.delete()
    return (f.getName(), true, exit_test, actual_output)
  }
}

def execute_tests(dir: String) = {
  if(DEBUG)
    print("testing directory: " + dir + "\n")
  val files = list_files("tests/" + dir)
  // for each file, run the compiler on it, run the jar, and check output
  val statuses = files.map(execute_test)
  print("\n")
  // filter out the successes
  val errors = statuses.filter(e => (e _1).length > 0)
  // display the errors
  errors.map(e => print("\t" + (e _1) +
                        ":\n\t\tCompiled: " + (e _2).toString +
                        "\n\t\tReturn Code: " + (e _3).toString +
                        "\n\t\tOutput: " +
                        """\n""".r.replaceAllIn((e _4), "\n\t\t\t") + "\n"))
  // display aggregate
  val err_len = errors.length
  val all_len = statuses.length
  print(err_len.toString + "/" + all_len.toString + " tests failed\n")
  (err_len, all_len)
}

// excecute all the tests across the categories
val all_results = cats.map(execute_tests)
val agg_results = all_results.reduceLeft((a,b) => ((a _1)+(b _1),(a _2)+(b _2)))
print("Total: " + (agg_results _1).toString + "/" +
      (agg_results _2).toString + " tests failed\n")
