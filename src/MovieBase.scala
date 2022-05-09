/** Class implemented by Cooper D. Cohen. */

final class MovieBase {

  private var mdb: Array[Movie] = _ //unknown size of lines

  def createDatabase(inpFile: String): Unit = {
    val raw = io.Source.fromFile(inpFile)
    val lines = raw.getLines().map(line => line.split('|'))
    mdb = new Array[Movie](lines.length+1) //create size of array same as number of movie entries. Movie index == mdb index.
    for (line <- lines) {
        mdb(line(0).toInt) = new Movie(line(0).toInt, line(1), line(2).substring(line(2).length-5).toInt, getGenre(line))
    }
  } /**reads and parses file. Must be called after instance creation. */

  def addRating(mind: Int, r: Int): Unit = {
    if (r > 0 && r < 6) mdb(mind).addRating(r)
  } /** add 1 rating to a particular movie, given index. */

  def rating(mind: Int): Double = {
    mdb(mind).rating
  } /**retrieves average rating of particular movie, given index. */

  def genre: Int = ??? /**need to collaborate to decide if genre should be returned as sequence or individually for each m.
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

  private class Movie(ind: Int, title: String, year: Int, genre: Array[Boolean]) {

    private var accRatings: Int = 0
    private var totRatings: Int = 0

    def addRating(r: Int):Unit = {
      if (r > 0 && r < 6) {
        totRatings += 1
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
  } /** internal Movie class for each movie entry. */
}
