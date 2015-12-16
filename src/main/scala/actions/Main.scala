package actions

import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

import slick.dbio.DBIOAction
import slick.profile.SqlAction

import slick.driver.H2Driver.api._

object Main {

  // Tables -------------------------------------

  case class Album(
    artist : String,
    title  : String,
    year   : Int,
    rating : Rating,
    id     : Long = 0L)

  class AlbumTable(tag: Tag) extends Table[Album](tag, "albums") {
    def artist = column[String]("artist")
    def title  = column[String]("title")
    def year   = column[Int]("year")
    def rating = column[Rating]("rating")
    def id     = column[Long]("id", O.PrimaryKey, O.AutoInc)

    def * = (artist, title, year, rating, id) <> (Album.tupled, Album.unapply)
  }

  lazy val AlbumTable = TableQuery[AlbumTable]



  // Schema actions -----------------------------

  val createTableAction =
    AlbumTable.schema.create

  val dropTableAction =
    AlbumTable.schema.drop



  // Select actions -----------------------------

  val selectAction: DBIOAction[Seq[String], NoStream, Effect.Read] =
    AlbumTable
      .filter(_.artist === "Keyboard Cat")
      .map(_.title)
      .result



  // Update actions -----------------------------

  val updateAction: DBIOAction[Int, NoStream, Effect.Write] =
    AlbumTable
      .filter(_.artist === "Keyboard Cat")
      .map(_.title)
      .update("Even Greater Hits")

  val updateAction2 =
    AlbumTable
      .filter(_.artist === "Keyboard Cat")
      .map(a => (a.title, a.year))
      .update(("Even Greater Hits", 2010))



  // Delete actions -----------------------------

  val deleteAction =
    AlbumTable
      .filter(_.artist === "Justin Bieber")
      .delete



  // Insert actions -----------------------------

  val insertOneAction : DBIOAction[Int, NoStream, Effect.Write] =
    AlbumTable += Album("Pink Floyd", "Dark Side of the Moon", 1978, Rating.Awesome )

  val insertAllAction : DBIOAction[Option[Int], NoStream, Effect.Write]  =
    AlbumTable ++= Seq(
      Album( "Keyboard Cat"  , "Keyboard Cat's Greatest Hits" , 2009 , Rating.Awesome ),
      Album( "Spice Girls"   , "Spice"                        , 1996 , Rating.Good    ),
      Album( "Rick Astley"   , "Whenever You Need Somebody"   , 1987 , Rating.NotBad  ),
      Album( "Manowar"       , "The Triumph of Steel"         , 1992 , Rating.Meh     ),
      Album( "Justin Bieber" , "Believe"                      , 2013 , Rating.Aaargh  ))



  // Database -----------------------------------

  val db = Database.forConfig("scalaxdb")


  // Exercises

  // Insert Three Albums favourite band
  val insertAlbumsFavouriteBand: DBIOAction[Option[Int], NoStream, Effect.Write] =
    AlbumTable ++= Seq(
      Album( "Els Pets"      , "BonDia"                     , 1997 , Rating.Awesome ),
      Album( "Els Pets"      , "Fràgil"                     , 2010 , Rating.NotBad ),
      Album( "Els Pets"      , "Com Anar al Cel i Tornar"   , 2007 , Rating.Meh )
    )

  def updateAlbumsAfterYear(year: Int): DBIOAction[Int, NoStream, Effect.Write] =
    AlbumTable
      .filter( _.year > year )
      .map(_.rating)
      .update( Rating.Meh )

  def deleteAlbumsByArtist(artist: String): DBIOAction[Int, NoStream, Effect.Write] =
    AlbumTable
      .filter( _.artist === artist )
      .delete


  def insertAlbumAction( artist: String, title: String, year: Int ) =
    for {
      isNotFirstAlbum <- AlbumTable.filter( _.artist === artist ).exists.result
      inserted     <- AlbumTable += Album( artist, title, year, { if(isNotFirstAlbum) Rating.Meh else Rating.Awesome })
    }
      yield inserted

  // Let's go! ----------------------------------

  def exec[T](action: DBIO[T]): T =
    Await.result(db.run(action), 2 seconds)

  def main(args: Array[String]): Unit = {
    exec(createTableAction)
    exec(insertAllAction)
    exec(insertOneAction)
    exec(selectAction).foreach(println)
    println("="*30)
    exec(insertAlbumsFavouriteBand)
    println("="*30)
    exec(updateAlbumsAfterYear(2000))
    println("="*30)
    exec(deleteAlbumsByArtist("Spice Girls"))

    exec(insertAlbumAction("Ferran", "First Album", 2015))
    exec(insertAlbumAction("Ferran", "Second Album", 2018))

    println("="*30)
    exec(AlbumTable.sortBy(_.year.asc).result).foreach(println)
  }


  def runManyActions() = {
    val actions =
      createTableAction andThen
      insertAllAction andThen
      insertOneAction

    exec(actions)
  }

}


/*

DBIOAction[ResultType, StreamingOrNotStreaming, Effect(read/writes/etc..)]
DBIOAction[Seq[Album], NoStream, Effect.All]

effect type, reasoning about what actions can be use in what context

we normally see

DBIO[ResultType] === DBIOAction[ResultType, NoStream, Effect.All]


also intersting
SqlAction[R, S, E]
  =>  myAction.statements


  on repl:  scala> updateAction2.statements // this will give us the sql query


 */