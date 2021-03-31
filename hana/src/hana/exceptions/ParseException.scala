package hana.exceptions

case class ParseException(message: String, cause: Throwable = None.orNull) extends Exception(message, cause)