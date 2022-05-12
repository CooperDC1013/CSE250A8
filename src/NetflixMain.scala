object NetflixMain extends App {

  val mdb = new MovieBaseCDC
  val udb = new Userbase_MM

  val mRatings: Array[Array[Int]] = udb.get_all_ratings()
  val users = udb.get_all_users()

  for (movie <- mRatings) {mdb.addRating(movie(0), movie(1))} //Add movie ratings to mdb to get later when discerning genres.

  var action: Int = 0
  var noir: Int = 1
  var light: Int = 2
  var serious: Int = 3
  var fantasy: Int = 4
  var history: Int = 5

  for (user <- users.indices) {

    val id: Int = users(user).userID
    val ratings: List[(Int, Int)] = users(id).get_users_ratings()


    val genreAcc: Array[Int] = Array.fill(6){0}
    val genreCount: Array[Int] = Array.fill(6){0}
    val movieAcc: Array[Int] = Array.fill(6){0}
    val movieCount: Array[Int] = Array.fill(6){0}



    for (rating <- ratings) {
      val genres: Array[Boolean] = mdb.genre(rating._1)

      for (g <- genres.indices) {
        if (genres(g)) {
          genreAcc(g) += rating._2
          genreCount(g) += 1
          //movieAcc(g) += mdb.rating(rating._1)
          movieAcc(g) += udb.compute_average_rating(rating._1)
          movieCount(g) += 1
        }
      }
    }

    val u_g: Array[Double] = (for (g <- genreAcc.indices) yield {genreAcc(g).toDouble / genreCount(g)}).toArray
    val r_g: Array[Double] = (for (r <- movieAcc.indices) yield {movieAcc(r).toDouble / movieCount(r)}).toArray
    val p_ug: Array[Double] = (for (p <- u_g.indices) yield {u_g(p) / r_g(p)}).toArray

    val allMovies = mdb.getMovies

    for (i <- allMovies.indices if i !=0) {
      val g: Array[Boolean] = mdb.genre(i)
      var score: Double = 0

      for (j <- g.indices if g(j)) {
        score = math.max(score, udb.compute_average_rating(i) * p_ug(j))
      }

      val rankings:
    }
  }
}
