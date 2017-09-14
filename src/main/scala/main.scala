import slick.jdbc.H2Profile.api._

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

object Example extends App {

  final case class User(lastName: String, id: Long = 0L)

  final class UserTable(tag: Tag) extends Table[User](tag, "app_user") {
    def id       = column[Long]("id", O.PrimaryKey, O.AutoInc)
    def lastName = column[String]("last_name")
    def * = (lastName, id).mapTo[User]
  }

  lazy val users = TableQuery[UserTable]

  implicit class RegexLikeOps(s: Rep[String]) {
    def regexLike(p: Rep[String]): Rep[Boolean] = {
      val expr = SimpleExpression.binary[String,String,Boolean] { (s, p, qb) =>
        qb.expr(s)
        qb.sqlBuilder += " ~* "
        qb.expr(p)
      }
      expr.apply(s,p)
    }
  }

  def find(names: Seq[String]): DBIO[Seq[String]] = {
    val pattern = names.mkString("|")
    users.filter(_.lastName regexLike pattern).map(_.lastName).result
  }

  val program = for {
    _ <- users.schema.create
    _ <- users ++= User("foo") :: User("baz") :: User("bar") :: Nil
    result <- find( Seq("baz","bar") )
  } yield result

 val db = Database.forConfig("example")
 println( Await.result(db.run(program), 2.seconds) )
 db.close()

}
