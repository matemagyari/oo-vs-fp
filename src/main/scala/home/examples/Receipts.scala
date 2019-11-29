package home.examples

trait DiskOperator {
  def eraseDisk(): Unit
  def archiveFiles(): Unit
}

object ReceiptsOOP {

  class Commander(diskOperator: DiskOperator) {

    def executeCommand(command: String): Unit = {

      //long and complex logic around authorization

      command match {
        case "erase-disk" ⇒ diskOperator.eraseDisk()
        case "archive-files" ⇒ diskOperator.archiveFiles()
      }
    }
  }
}

object ReceiptsFP {

  sealed trait DiskOperations
  case object EraseDisk extends DiskOperations
  case object ArchiveFiles extends DiskOperations

  def interpretCommand(command: String): DiskOperations = {

    //long and complex logic around authorization

    command match {
      case "erase-disk" ⇒ EraseDisk
      case "archive-files" ⇒ ArchiveFiles
    }
  }

  class Commander(diskOperator: DiskOperator) {

    def executeCommand(command: String): Unit = {
      interpretCommand(command) match {
        case EraseDisk ⇒ diskOperator.eraseDisk()
        case ArchiveFiles ⇒ diskOperator.archiveFiles()
      }
    }
  }
}
