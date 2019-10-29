import cats.data.Validated.Valid
import jto.validation._
import org.scalatest.{BeforeAndAfterAll, FlatSpec, Matchers}
import play.api.libs.json._

class ValidationSpec extends FlatSpec with Matchers with BeforeAndAfterAll {

  val js = Json.parse("""{
  "user": {
    "name" : "toto",
    "age" : 25,
    "email" : "toto@jmail.com",
    "isAlive" : true,
    "friend" : {
      "name" : "tata",
      "age" : 20,
      "email" : "tata@coldmail.com"
    }
  }
    }""")
  val user = Json.parse("""{
      |    "name" : "toto",
      |    "age" : 25,
      |    "email" : "toto@jmail.com",
      |    "isAlive" : true
      |    }
      |""".stripMargin)

    case class User(name: String, age: Int, email: Option[String], isAlive: Boolean)

  it should "Validating and transforming data" in {
    import jto.validation.playjson.Rules._
    val age: Rule[JsValue, Int] = (Path \ "user" \ "age").from[JsValue](min(0) |+| max(130))
    age.validate(js) shouldBe Valid(25)
  }

  it should "Combining Rules" in {
    import jto.validation.playjson.Rules._
    val userRule: Rule[JsValue, User] = From[JsValue]{ __ =>
      ( (__ \ "name").read[String] ~
        (__ \ "age").read[Int] ~
        (__ \ "email").read[Option[String]] ~
        (__ \ "isAlive").read[Boolean]
        )(User.apply)
    }
    userRule.validate(user) shouldBe Valid(User("toto", 25, Some("toto@jmail.com"), true))
  }

  it should "Serializing data" in {
      def currency: Write[Double, String] = Write[Double, String]{ (money: Double) =>
        import java.text.NumberFormat
        import java.util.Locale

        val format = NumberFormat.getCurrencyInstance(Locale.FRANCE)
        format.format(money)
      }
    currency.writes(9.99) shouldBe "9,99 €"

    case class Product(name: String, price: Double)
    val productPrice: Write[Product, Double] = Write[Product, Double](_.price)
    val productAsPrice: Write[Product, String] = productPrice andThen currency
    productAsPrice.writes(Product("Laptop", 900.99d)) shouldBe "900,99 €"
    }

  it should "Combining Writes with primitive types" in {
    import jto.validation.playjson.Writes._
    val serializeFriend = (Path \ "user" \ "friend").write[JsValue, JsObject]
    serializeFriend.writes(JsString("titi")) should be ( JsObject(Map("user" -> JsObject(Map("friend" -> JsString("titi"))))) )
  }

  it should "Combining Writes with Type coercion" in {
    import jto.validation.playjson.Writes._
    val serializeFriend: Write[Int, JsObject] = (Path \ "user" \ "age").write[Int, JsObject]
    serializeFriend.writes(30) should be ( JsObject(Map("user" -> JsObject(Map("age" -> JsNumber(30))))) )
  }

  it should "Combining Writes with proper types" in {
    val userWrite: Write[User, JsObject] = To[JsObject] { __ =>
      import jto.validation.playjson.Writes._
      import scala.Function.unlift
      (
        (__ \ "name").write[String] ~
        (__ \ "age").write[Int] ~
        (__ \ "email").write[Option[String]] ~
        (__ \ "isAlive").write[Boolean]
      )(unlift(User.unapply))
    }
    userWrite.writes(User("toto", 30, Some("toto@email.com"), true)) shouldBe ( JsObject(Map("name" -> JsString("toto"), "age" -> JsNumber(30), "email" -> JsString("toto@email.com"), "isAlive" -> JsBoolean(true) )) )
  }
}
