/** Class implemented by Cooper D. Cohen. */

final class MovieBaseCDC {

  private var mdb: Array[Movie] = Array.tabulate(1683)(_ => dummyMovie()) //unknown size of lines
  createDatabase("./src/u.item.txt")

  private def dummyMovie(): Movie = new Movie(0, "", 0, Array(true, true, true, true, true, true))

  def createDatabase(inpFile: String): Unit = {
    val raw = io.Source.fromFile(inpFile)
    val lines = raw.getLines().map(line => line.split('|'))
    //mdb = new Array[Movie](lines.length+1) //create size of array same as number of movie entries. Movie index == mdb index.
    for (line <- lines if line.length != 1 && line(1).compareTo("unknown") != 0 && line(1)(line(1).length - 2) != 'V') {
      //println(line.mkString("Array(", ", ", ")"))
      //mdb(line(0).toInt) = new Movie(line(0).toInt, line(1), line(2).substring(line(2).length-5).toInt, getGenre(line))
      mdb(line(0).toInt) = new Movie(line(0).toInt, line(1), line(1).trim.drop(line(1).length-6).drop(1).dropRight(1).toInt, getGenre(line))

    }
  } /**reads and parses file. Must be called after instance creation. */

  def getMovies: Array[Movie] = mdb.clone()

  def getTitle(mind: Int) = mdb(mind).title

  def addRating(mind: Int, r: Int, a: Int = 1): Unit = {
    if (r > 0 && r < 6) mdb(mind).addRating(r, a)
  } /** add 1 rating to a particular movie, given index. */

  def rating(mind: Int): Double = {
    mdb(mind).rating
  } /**retrieves average rating of particular movie, given index. */

  def genre(mind: Int): Array[Boolean] = {
    mdb(mind).getGenre
  } /**need to collaborate to decide if genre should be returned as sequence or individually for each m.
   Method used to retrive genre of either a particular movie, or given a list of indices returns buckets for each genre.
  */
  private def getGenre(lst: Array[String]): Array[Boolean] = {

    val action: Boolean = (lst(6) + lst(7) + lst(21) + lst(23)).toInt > 0
    val noir: Boolean = (lst(11) + lst(15) + lst(16) + lst(18)).toInt > 0
    val light: Boolean = (lst(8) + lst(9) + lst(10) + lst(17)).toInt > 0
    val serious: Boolean = (lst(13) + lst(19)).toInt > 0
    val fantasy: Boolean = (lst(20) + lst(14)).toInt > 0
    val history: Boolean = (lst(12) + lst(22)).toInt > 0

    Array(action, noir, light, serious, fantasy, history)
  } /** converts genre string from file into array */

  class Movie(val ind: Int, val  title: String, val year: Int, val genre: Array[Boolean]) {

    private var accRatings: Int = 0
    private var totRatings: Int = 0

    def addRating(r: Int, a: Int = 1):Unit = {
      if (r > 0 && r < 6) {
        totRatings += a
        accRatings += r
      }
    }

    def rating: Double = {
      val av: Double = accRatings.toDouble / totRatings
      av
    }

    def isGenre(g: String): Boolean = {
      g.toLowerCase() match {
        case "action" => genre(0)
        case "noir" => genre(1)
        case "light" => genre(2)
        case "serious" => genre(3)
        case "fantasy" => genre(4)
        case "history" => genre(5)
      }
    }

    def getGenre: Array[Boolean] = genre
  } /** internal Movie class for each movie entry. */
}
