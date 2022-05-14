/** Object NetflixMain by Cooper Cohen + Michael McDonald for CSE250, Spring 2022.
 * Outputs MovieLens recommendations for every user to a text file. An optional command line argument can be used
 * to specify how many recommendations should be outputted for each user, else a default of 10 is used
 */

import java.io.FileWriter

object NetflixMain extends App {
  // Heap for storing each user's recommendations; a new heap is used for each user
  // The cap is 1682 since that's the total number of movies
  private class Recommendations extends Heap[(String, Double)](1682, (x, y) => x._2.compareTo(y._2))

  private val moviebase = new MovieBaseCDC
  private val userbase = new Userbase_MM

  // Allows an optional command line argument for the number of recommendations provided
  private def number_of_recommendations(): Int = {
    if (args.length == 1) {
      if (!args(0).forall(_.isDigit) || !userbase.valid_movie_id(args(0).toInt)) {
        return -1
      }
      else return args(0).toInt
    }
    10
  }
  var recommendations_count = number_of_recommendations()
  if (recommendations_count == -1) {
    println("Invalid input; please try again with a number between 1 and 1682.")
    println("A default of 10 will be used instead.\n")
    recommendations_count = 10
  }

  private val allMovies = moviebase.getMovies
  private val allUsers = userbase.get_all_users()

  // The recommendations are written to an output file
  val writer = new FileWriter("output.txt", false)

  // Loop over all users and print recommendations for each user
  for (userID <- allUsers.indices if userID != 0) {
    // Heap used to store a user's recommendation scores; convenient for popping at the end to get recommendations
    val rankings = new Recommendations()

    // (Int, Int) = (movieID, rating)
    val ratings: List[(Int, Int)] = allUsers(userID).get_users_ratings()

    // Accumulates the user's total rating and number of ratings submitted for each genre
    val genreAcc: Array[Int] = Array.fill(6){0}
    val genreCount: Array[Int] = Array.fill(6){0}
    // Accumulates the average rating of all movies for each genre; used to calculate p_ug for each user
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

    // Loop over all movies and generate a recommendation score for the user for each movie.
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
    for (x <- 0 until recommendations_count) {
      writer.write(s"#${x+1} recommendation for user #${allUsers(userID).userID} is: ${rankings.pop()}\n")
    }
    writer.write("\n")
  }
  writer.close()
}