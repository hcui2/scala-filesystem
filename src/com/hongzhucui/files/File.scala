package com.hongzhucui.files

import com.hongzhucui.filesystem.FileSystemException

class File(override val parentPath: String, override val name: String, val contents: String)
  extends DirEntry(parentPath, name) {



  override def asDirectory: Directory = throw new FileSystemException("A file cannot be converted to a directory")

  override def asFile: File = this

  override def getType: String = "File"

  override def isDirectory: Boolean = false
  override def isFile: Boolean = true


  def appendContents(newContents: String): File =
    setContents(contents + "\n" + newContents)

  def setContents(newContents: String): File =
    new File(parentPath, name, newContents)
}

object File {
  def empty(parentPath: String, name: String) : File =
    new File(parentPath, name, "")

}