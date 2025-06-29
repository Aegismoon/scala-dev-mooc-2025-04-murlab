package ru.otus.module2

object murscala3js {

    sealed trait JsValue
  
      final case class JsObject(get: Map[String, JsValue]) extends JsValue

      final case class JsString(get: String) extends JsValue

      final case class JsNumber(get: Double) extends JsValue

      final case object JsNull extends JsValue
    


    trait JsonWriter[T] {
      def write(v: T): JsValue
    }

    object JsonWriter {

      def apply[T](implicit ev: JsonWriter[T]): JsonWriter[T] = ev

      def from[T](f: T => JsValue) = new JsonWriter[T] {
        override def write(v: T): JsValue = f(v)
      }

      implicit val inJsonWriter = from[Int](JsNumber(_))

      implicit val strJsonWriter = from[String](JsString)

      implicit def optToJsValue[T](implicit ev: JsonWriter[T]) =
        from[Option[T]] {
          case Some(value) => ev.write(value)
          case None => JsNull
        }
    }

    implicit class JsonSyntax[T](v: T) {
      def toJson(implicit ev: JsonWriter[T]): JsValue = ev.write(v)
    }


    def toJson[T: JsonWriter](v: T): JsValue = {
      JsonWriter[T].write(v)
    }


    toJson("fdvvbfbv")
    toJson(10)
    toJson(Option(10))
    toJson(Option("dfbgfgnhg"))

    "bghbbgfrbgbngf".toJson
    Option(10).toJson


}
