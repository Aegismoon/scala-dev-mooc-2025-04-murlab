package ru.otus.module2

object murscala3js {

    sealed trait JsValue

      final case class JsObject(get: Map[String, JsValue]) extends JsValue

      final case class JsString(get: String) extends JsValue

      final case class JsNumber(get: Double) extends JsValue

      final case object JsNull extends JsValue



    trait JsonWriter[T]:
      def write(v: T): JsValue


    object JsonWriter:
      def apply[T](using ev: JsonWriter[T]): JsonWriter[T] = ev
      def from[T](f: T => JsValue) = new JsonWriter[T]:
        override def write(v: T): JsValue = f(v)

      // инстансы
      given  inJsonWriter: JsonWriter[Int] = from[Int](JsNumber(_))
      given  strJsonWriter: JsonWriter[String] = from[String](JsString)

      given [T: JsonWriter]: JsonWriter[Option[T]] = from {
      case Some(value) => summon[JsonWriter[T]].write(value)
      case None => JsNull
      }
    extension [T](v: T) def toJson(using ev: JsonWriter[T]): JsValue = ev.write(v)


    //def toJson[T: JsonWriter](v: T): JsValue = summon[JsonWriter[T]].write(v)
    @main def murRunJsonExamples(): Unit =
    // Примеры использования
    toJson("murtest") // JsString("mutest")
    toJson(42) // JsNumber(42.0)
    toJson(Option(10)) // JsNumber(10.0)
    "hello".toJson // JsString("hello")
    Option.empty[Int].toJson // JsNull

}
