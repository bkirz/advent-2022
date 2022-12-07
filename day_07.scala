import scala.io.Source
import javax.swing.plaf.RootPaneUI

sealed trait FSNode
case object DirNode extends FSNode
case class FileNode(size: Int) extends FSNode

// Not trying to do any fancy tree modeling here. Just
// a map of path as list of dir names to nodes. This encodes a bunch of
// redundant info, but that's ok. This should all fit in memory and should be
// efficient enough.
case class FilesystemState(
    nodes: Map[List[String], FSNode],
    currentDir: List[String]
) {
  def dirSize(path: List[String]): Int =
    nodes
      .filter((p, _) => p.startsWith(path))
      .collect { case (_, FileNode(size)) => size }
      .sum

  def dirPaths: List[List[String]] =
    nodes.collect { case (path, DirNode) => path }.toList
}

sealed trait ShellCommand
case class CDCommand(target: CDTarget) extends ShellCommand

sealed trait CDTarget
case object Root extends CDTarget
case object Parent extends CDTarget
case class Child(name: String) extends CDTarget

case class LSCommand(output: List[LSOutput]) extends ShellCommand

sealed trait LSOutput
case class FileDescription(name: String, size: Int) extends LSOutput
case class DirDescription(name: String) extends LSOutput

val DIR_SIZE_THRESHOLD = 100_000

object Day07 {
  @main def main = {
    val lines = Source.fromFile("day_07.input").getLines().toList
    val commands = parseCommands(lines)

    val initialFsState = FilesystemState(
      nodes = Map(List() -> DirNode),
      currentDir = List()
    )
    val finalFsState = commands.foldLeft(initialFsState)(processCommand)
    val part1Solution =
      finalFsState.dirPaths
        .map(path => finalFsState.dirSize(path))
        .filter(_ <= DIR_SIZE_THRESHOLD)
        .sum
    println(f"Part 1: $part1Solution")
  }

  val CD_PATTERN = """\$ cd (/|\.\.|\w+)""".r
  val LS_COMMAND = "$ ls"
  val LS_DIR_LINE_PATTERN = """dir (\w+)""".r
  val LS_FILE_LINE_PATTERN = """(\d+) ([a-z.]+)""".r
  def parseCommands(lines: List[String]): List[ShellCommand] = {
    if (lines.isEmpty) { return List.empty }

    val (List(command), rest) = lines.splitAt(1)
    command match {
      case CD_PATTERN(targetStr) =>
        val target = targetStr match {
          case "/"  => Root
          case ".." => Parent
          case name => Child(name)
        }
        CDCommand(target) :: parseCommands(rest)

      case LS_COMMAND =>
        val (lsOutputLines: List[String], tail: List[String]) =
          rest.span { (lsLine: String) =>
            LS_DIR_LINE_PATTERN.matches(lsLine) ||
            LS_FILE_LINE_PATTERN.matches(lsLine)
          }
        LSCommand(lsOutputLines.map(parseLSOutputLine)) :: parseCommands(tail)
    }
  }

  def parseLSOutputLine(line: String): LSOutput =
    line match {
      case LS_DIR_LINE_PATTERN(dirName) => DirDescription(dirName)
      case LS_FILE_LINE_PATTERN(size, fileName) =>
        FileDescription(fileName, size.toInt)
    }

  def processCommand(
      currentState: FilesystemState,
      command: ShellCommand
  ): FilesystemState = {
    command match {
      case CDCommand(Root) => currentState.copy(currentDir = List())
      case CDCommand(Parent) =>
        currentState.copy(currentDir = currentState.currentDir.dropRight(1))
      case CDCommand(Child(name)) =>
        val path = currentState.currentDir :+ name
        currentState.copy(
          currentDir = path,
          nodes = currentState.nodes + (path -> DirNode)
        )
      case LSCommand(output) =>
        val newNodes = output.map {
          case DirDescription(name) =>
            (currentState.currentDir :+ name, DirNode)
          case FileDescription(name, size) =>
            (currentState.currentDir :+ name, FileNode(size))

        }
        currentState.copy(nodes = currentState.nodes ++ newNodes)
    }
  }

}
