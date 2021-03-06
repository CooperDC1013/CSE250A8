/** Object NetflixMain by Cooper Cohen + Michael McDonald for CSE250, Spring 2022.
 * Rather than outputting recommendations for all users, print recommendations based on client input.
 * The client is asked to enter a userID and number of desired recommendations.
 */
object NetflixMain2 {
  def main(args: Array[String]): Unit = {
    val moviebase = new MovieBaseCDC
    val userbase = new Userbase_MM

    // Client input
    print("Enter a user ID: ")
    val id_input = scala.io.StdIn.readLine()
    if (!id_input.forall(_.isDigit) || !userbase.valid_user_id(id_input.toInt)) {
      println("Invalid ID; please try again with a number between 1 and 943.")
      return
    }
    val userID = id_input.toInt

    print("Enter a number of recommendations: ")
    val recommendations_input = scala.io.StdIn.readLine()
    if (!recommendations_input.forall(_.isDigit) || !userbase.valid_movie_id(recommendations_input.toInt)) {
      println("Invalid input; please try again with a number between 1 and 1682")
      return
    }
    println()


    // Heap for storing each user's recommendations
    class Recommendations extends Heap[(String, Double)](1682, (x, y) => x._2.compareTo(y._2))
    val rankings = new Recommendations()

    val allMovies = moviebase.getMovies
    val allUsers = userbase.get_all_users()

    // (Int, Int) = (movieID, rating)
    val ratings: List[(Int, Int)] = allUsers(userID).get_users_ratings()

    // Accumulates the user's total rating and number of ratings submitted for each genre
    val genreAcc: Array[Int] = Array.fill(6){0}
    val genreCount: Array[Int] = Array.fill(6){0}
    // Accumulates the average rating of all movies for each genre; used to calculate p_ug
    val movieAcc: Array[Double] = Array.fill(6){0}
    val movieCount: Array[Int] = Array.fill(6){0}

    // Loop over all of a user's ratings to calculate u_g and u_gAverage
    for (rating <- ratings) {
      val genres: Array[Boolean] = moviebase.genre(rating._1)
      for (g <- genres.indices) {
        if (genres(g)) {
          genreAcc(g) += rating._2
          genreCount(g) += 1

          movieAcc(g) += userbase.compute_average_rating(rating._1)
          movieCount(g) += 1
        }
      }
    }
    val u_g: Array[Double] = (for (g <- genreAcc.indices) yield {genreAcc(g).toDouble / genreCount(g)}).toArray
    val u_gAverage: Array[Double] = (for (g <- movieAcc.indices) yield {movieAcc(g) / movieCount(g)}).toArray

    // Stores the p_ug of all 6 genres for the current user
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
    for (x <- 0 until recommendations_input.toInt) {
      println(s"#${x+1} recommendation for user #${allUsers(userID).userID} is: ${rankings.pop()}")
    }
  }
}