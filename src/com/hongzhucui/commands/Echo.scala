package com.hongzhucui.commands
import com.hongzhucui.files.{Directory, File}
import com.hongzhucui.filesystem.State

class Echo(args: Array[String]) extends Command {
  override def apply(state: State): State = {
    /*
    if no argument, same state
    else if one arg, print to console
    else multiple args :
      [
        operator > echo to a file, create a file if not there
        >> append to a file

        else : print
     */
    if (args.isEmpty) state
    else if (args.length == 1) state.setMessage(args(0))
    else {
      val operator = args(args.length - 2)
      val filename = args(args.length - 1)
      val contents = createContent(args, args.length - 2)

      if (">>".equals(operator))
        doEcho(state, contents, filename, append = true)
      else if (">".equals(operator)){
        doEcho(state, contents, filename, append = false)

      }
      else
        state.setMessage(createContent(args, args.length))
    }
  }
  // topInex non-inclusive
  def createContent(args: Array[String], topInex: Int):String = {
    def createContentHelper(currentIndex: Int, accumulator: String): String= {
      if (currentIndex >= topInex) accumulator
      else createContentHelper(currentIndex + 1, accumulator + " " + args(currentIndex))
    }
    createContentHelper(0, "")
  }

  def getRootAfterEcho(currentDirectory: Directory, path: List[String], contents: String, append: Boolean): Directory = {
    /*
    if the path is empty : fail:
    else if no more things to explore
      if the path tail is empty: find the file to create / add content to
      if file not found, create a file
      else if the entry is a directory, fail
      else :
     else:
      find the next directory to navigate

     */
    if (path.isEmpty) currentDirectory
    else if (path.tail.isEmpty) {
      val dirEntry = currentDirectory.findEntry(path.head)

      if (dirEntry == null)
        currentDirectory.addEntry(new File(currentDirectory.path, path.head, contents))
      else if (dirEntry.isDirectory) currentDirectory
      else if (append) currentDirectory.replaceEntry(path.head, dirEntry.asFile.appendContents(contents))
      else
        currentDirectory.replaceEntry(path.head, dirEntry.asFile.setContents(contents))
    }
    else {
      val nextDirectory = currentDirectory.findEntry(path.head).asDirectory
      val newNextDirectory = getRootAfterEcho(nextDirectory, path.tail, contents, append)

      if (newNextDirectory == nextDirectory) currentDirectory
      else currentDirectory.replaceEntry(path.head, newNextDirectory)


    }
  }

  def doEcho(state: State, contents: String, filename: String, append: Boolean): State = {
    if (filename.contains(Directory.SEPARATOR) ){
      state.setMessage("Echo: not supporrint separator now")

    }
    else {
      val newRoot: Directory = getRootAfterEcho(state.root, state.wd.getAllFoldersInPath :+ filename, contents, append)
      if (newRoot == state.root)
        state.setMessage(filename + ": no such file")
      else
        State(newRoot, newRoot.findDescendant(state.wd.getAllFoldersInPath))
    }

  }

}

