/** Object NetflixMain by Cooper Cohen + Michael McDonald for CSE250, Spring 2022.
 * Rather than outputting recommendations for all users, print recommendations based on client input.
 * The client is asked to enter a userID and number of desired recommendations.
 */
object NetflixMain2 {
  def main(args: Array[String]): Unit = {
    print("Enter a user ID: ")
    val id_input = scala.io.StdIn.readLine().toInt
    print("Enter a number of recommendations: ")
    val recommendations_input = scala.io.StdIn.readLine().toInt
    println()


    // Heap for storing each user's recommendations; a new heap is used for each user
    class Recommendations extends Heap[(String, Double)](1682, (x, y) => x._2.compareTo(y._2))
    val rankings = new Recommendations()

    val moviebase = new MovieBaseCDC
    val userbase = new Userbase_MM

    val allMovies = moviebase.getMovies
    val allUsers = userbase.get_all_users()
    val allRatings = userbase.get_all_ratings()

    // Accumulates the average rating of all movies for each genre; used to calculate p_ug
    val movieAcc: Array[Double] = Array.fill(6){0}
    val movieCount: Array[Int] = Array.fill(6){0}
    // Loop over all movies and compute the average rating for each genre.
    for (movieID <- allMovies.indices if movieID != 0) {
      val genres: Array[Boolean] = moviebase.genre(movieID)
      val overall_rating = allRatings(movieID)
      for (g <- genres.indices) {
        if (genres(g)) {
          movieAcc(g) += overall_rating(0)
          movieCount(g) += overall_rating(1)
        }
      }
    }
    val u_gAverage: Array[Double] = (for (g <- movieAcc.indices) yield {movieAcc(g) / movieCount(g)}).toArray


    // (Int, Int) = (movieID, rating)
    val ratings: List[(Int, Int)] = allUsers(id_input).get_users_ratings()

    // Accumulates the user's total rating and number of ratings submitted for each genre
    val genreAcc: Array[Int] = Array.fill(6){0}
    val genreCount: Array[Int] = Array.fill(6){0}
    // Loop over all of a user's ratings to calculate u_g
    for (rating <- ratings) {
      val genres: Array[Boolean] = moviebase.genre(rating._1)
      for (g <- genres.indices) {
        if (genres(g)) {
          genreAcc(g) += rating._2
          genreCount(g) += 1
        }
      }
    }

    // Stores the p_ug of all 6 genres for the current user
    val u_g: Array[Double] = (for (g <- genreAcc.indices) yield {genreAcc(g).toDouble / genreCount(g)}).toArray
    val p_ug: Array[Double] = (for (g <- u_g.indices) yield {u_g(g) / u_gAverage(g)}).toArray

    // Loop over all movies and generate a recommendation score for the user for each movie
    // Then insert the recommendations in a maxHeap for sortedness
    for (movieID <- allMovies.indices if movieID != 0) {
      val genres: Array[Boolean] = moviebase.genre(movieID)
      var score: Double = 0

      for (j <- genres.indices if genres(j)) {
        // Calculate a different recommendation score for each genre based on p_ug and take the maximum value
        score = math.max(score, userbase.compute_average_rating(movieID) * p_ug(j))
      }
      rankings.enqueue((allMovies(movieID).title, score))
    }
    // Pop the top 10, 30, 50, etc. recommendations from the heap
    // Write the recommendations to an output file
    for (x <- 0 until recommendations_input) {
      println(s"#${x+1} recommendation for user #${allUsers(id_input).userID} is: ${rankings.pop()}")
    }
  }
}