/** File "Userbase.scala" by Michael McDonald for CSE250, Spring 2022.
 *
 *  Reads the GroupLens user data from u.data and u.user, storing it in the Userbase class.
 *  Methods are provided for querying user data and movie rating data.
 *
 *  Uses an array to store each user and a hash table to store a collection of ratings for each user.
 *  Additionally, an array is used to store each movie with its total rating (the sum of all user ratings) and the
 *  number of ratings submitted for that movie.
 */
import io.Source

class Userbase_MM {
  private val MINIMUM_USER_ID = 1
  private val MAXIMUM_USER_ID = 943
  private val MINIMUM_MOVIE_ID = 1
  private val MAXIMUM_MOVIE_ID = 1682

  /* Primary fields for the Userbase.

   * users = Array[User]. Since user ID's are in the range 1-943 (inclusive), an Array is convenient for O(1) access.
   * users[1] is user1, users[2] is user2, etc. Index 0 is unused since 0 isn't a valid ID

   * movie_ratings = Array[Array[Int]]. The outer array stores all 1682 movies. Similarly to user IDs, movie IDs are
   * in the range 1-1682 (inclusive), so an Array is convenient for O(1) access. Index 0 is unused.
   * The inner Array[Int] store 2 integers: (total rating, number of ratings) where the total rating is the sum of all
   * ratings users have submitted for the given movie; this is useful for calculating a movie's avg rating efficiently.
   */
  private val _users_MM: Array[User] = Array.tabulate(MAXIMUM_USER_ID+1)(n => new User(n))
  private val _movie_ratings_MM: Array[Array[Int]] = Array.ofDim[Int](MAXIMUM_MOVIE_ID+1, 2)
  userbase_init() // Parses u.data amd u.user to extract the relevant data



  /* Hash[Tuple[Int]] where Tuple=(movieID, user rating). Used for the ratings field in the User class.
   * The hash is based only on the movieID; this is sufficient since each movie has a unique ID.
   * A single user is unlikely to rate over 600 movies, so use around 700 buckets
   */
  protected class User_RatingsHash_MM extends HashISR[(Int, Int)](numSlots = 700, x => x._1.hashCode(),
    (x, y) => x._1 == y._1)


////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Start of internal User clas

  /* Store's a user's ID and the ratings they've submitted as described above.
   * Also stores demographics information about the user.
   */
  class User(val userID: Int) {
    private val _ratings_MM = new User_RatingsHash_MM()
    private val _demographics = Array("", "", "") // age, gender, occupation


    /* Inserts a rating into the user's ratings hash table.
     * To avoid double insertions for the same movie, a check for the rating is first performed.
     * For an invalid movie ID or rating, do nothing.
     * As is, allows for insertion of ratings beyond those initially given, but be mindful of the number of buckets
     * that the _ratings hash table has.
     */
    def insert_rating(movie_id: Int, rating: Int): Unit = {
      if (! valid_movie_id(movie_id) || rating < 1 || rating > 5) return
      if (!_ratings_MM.contains((movie_id, rating))) _ratings_MM.insert((movie_id, rating))
    }

    /* Retrieve the user's rating for a given movie.
     * If the user doesn't have a rating for the movie, or the movie ID is invalid, return -1
     * Note: a dummy tuple with the given ID is used for the search since the ratings hash is based only on the ID.
     */
    def get_rating(movie_id: Int): Int = {
      if (! valid_movie_id(movie_id)) return -1
      val ratings_iter = _ratings_MM.find((movie_id, 0))
      if (ratings_iter.hasNext) ratings_iter.apply()._2 else -1
    }


    /* Return all of the ratings submitted by the user as a list.
     * Useful for looping through a user's ratings to calculate their
     * genre ratings.
     */
    def get_users_ratings(): List[(Int, Int)] = {
      var result: List[(Int, Int)] = List()
      val ratings_rover = _ratings_MM.begin
      while (ratings_rover.hasNext) {
        val rating = ratings_rover.apply()
        result ::= rating
        ratings_rover.next()
      }
      result
    }


    /* Returns the number of ratings submitted by the user
     */
    def ratings_count(): Int = _ratings_MM.size


    /* Prints a user's demographic info as found in the u.user file.
     * Only meaningful if get_demographics() is called first.
     * Prints the user's age, gender, and occupation; occupations entered as other/none are omitted.
     */
    def print_demographics(): Unit = {
      if (_demographics.head.compareTo("") == 0) println(s"Info unavailable for user $userID.")
      val his_her = _demographics(1) match {
        case "F" => "Her"
        case "M" => "His"
      }
      val result: String = _demographics(2) match {
        case "other" | "none" => s"User $userID is a ${_demographics.head} year old ${_demographics(1)}."
        case _ => s"User $userID is a ${_demographics.head} year old ${_demographics(1)}. " +
          s"$his_her occupation is: ${_demographics(2)}."
      }
      println(result)
    }

    /* Sets the User's _demographics field. Called by get_demographics() when u.user is first read in userbase_init().
     * Currently not meant to be called manually, so calls after the first initialization simply do nothing and return.
     * For the future, maybe allow the addition of more users to the userbase and then call this method for a new user.
     */
    def initialize_demographics(age: String, gender: String, occupation: String): Unit = {
      if (_demographics.head.compareTo("") != 0) return // already initialized
      _demographics(0) = age
      _demographics(1) = gender
      _demographics(2) = occupation

    }
  }
  // End of internal User class
///////////////////////////////////////////////////////////////////////////////////////////////////////////



///////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Start of private methods

  /* Makes a dummy user. Used for returning a dummy with an ID of -1 from the get_user() method upon a failed lookup
   * in order to satisfy the method's return type.
   */
  private def dummy_user(): User = new User(-1)

  /* Extracts data from the u.user file and stores it in a demographics field in the User class.
   * The users array must be initialized first due to the call to get_user() and initialize_demographics()
   */
  private def get_demographics(): Unit = {
    val userdata_file = "./src/u.user.txt"
    val source = Source.fromFile(userdata_file)
    val userinfo_list = source.getLines().map(line => line.split("\\|")).toList
    source.close()

    for (entry <- userinfo_list) {
      val id = entry.head.toInt
      if (!valid_user_id(id)) return

      val age = entry(1)
      val gender = entry(2)
      val occupation = entry(3)

      val user = get_user(id)
      user.initialize_demographics(age, gender, occupation)

    }
  }


  /* Parse the u.data file to setup the users array and ratings hash table for each user.
   * Also sets up the movie_ratings array to make it easy to calculate a movie's average rating.
   * Finally, parse the u.user file to get demographics information about each user.
   * Called whenever an instance of the class is made.
   */
  private def userbase_init(): Unit = {
    val userdata_file = "./src/u.data.txt"
    val source = Source.fromFile(userdata_file)
    val userdata_list = source.getLines().map(line => line.split("\\t")).toList
    source.close()

    for (entry <- userdata_list) {
      val user_id = entry.head.toInt
      val movie_id = entry(1).toInt
      val rating = entry(2).toInt

      // (1) Update movie_ratings table
      _movie_ratings_MM(movie_id)(0) += rating
      _movie_ratings_MM(movie_id)(1) += 1

      // (2) Update user's ratings table
      _users_MM(user_id).insert_rating(movie_id, rating)
    }

    //get_demographics() // parses u.user
  }
  // End of private methods
///////////////////////////////////////////////////////////////////////////////////////////////////////////



////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Start of client methods

  /* Clone the user's array. Used for iterating over all users since the _users
   * array is private; prevents the user from altering _users
   */
  def get_all_users(): Array[User] = _users_MM.clone() // users[0] is unused; can retrieve ID from User class


  /* Clone the ratings array. Used for iterating over all ratings since the _movie_ratings
   * array is private; prevents the user from altering _movie_ratings
   */
  def get_all_ratings(): Array[Array[Int]] = _movie_ratings_MM.clone() // index 0 is unused, but keep so index = movieID


  /* Fetch the User with the given ID from the users hash table.
   * If the user doesn't exist, return a dummy user whose ID is -1.
   */
  def get_user(id: Int): User = {
    if (valid_user_id(id)) return _users_MM(id)
    dummy_user()
  }


  /* Returns the number of ratings submitted for a given movie.
   * If the given movie ID is invalid, return -1
   */
  def ratings_count(movie_id: Int): Int = {
    if (valid_movie_id(movie_id)) _movie_ratings_MM(movie_id)(1)
    else -1
  }


  /* Given a movie's ID, return the average user rating for that movie.
   * Consults the movie_ratings array for quick access to the data needed to make the calculation.
   * If the ID is invalid, or the given movie has no ratings, return -1.
   */
  def compute_average_rating(movie_id: Int): Double = {
    if (! valid_movie_id(movie_id)) return -1
    if (_movie_ratings_MM(movie_id)(0) == 0) return -1 // no ratings submitted
    _movie_ratings_MM(movie_id)(0) / _movie_ratings_MM(movie_id)(1).toDouble
  }


  /* Given a userID and a movieID, return the user's rating for that movie.
   * If the userID/movieID is invalid, return -1; if the user doesn't have a rating for the given movie, return -1
   */
  def rating_query(user_id: Int, movie_id: Int): Int = {
    if (! valid_user_id(user_id) || ! valid_movie_id(movie_id)) return -1
    _users_MM(user_id).get_rating(movie_id)
  }


  /* Returns the number of users in the Userbase.
   * -1 to omit users[0]
   */
  def size(): Int = _users_MM.length - 1


  /* Checks whether a given userID is valid.
   * Returns true if it's valid and false otherwise
   */
  def valid_user_id(id: Int): Boolean = id >= MINIMUM_USER_ID && id <= MAXIMUM_USER_ID

  /* Checks whether a given movieID is valid.
   * Returns true if it's valid and false otherwise
   */
  def valid_movie_id(id: Int): Boolean = id >= MINIMUM_MOVIE_ID && id <= MAXIMUM_MOVIE_ID

}


/* Performs 4 timings related to Userbase
 * (1) time to initialize (userbase_init)
 * (2) time to call get_user() on every user in the table
 * (3) time to compute average rating for all movies
 * (4) time to call rating_query(user, movie) for every user and every movie
 */
/*object TimingTest extends App {
  println("Timing test results:")
  val ms = 1000000.0
  var elapsedTime = 0.0
  var totalTime = 0.0


  // init test
  var t1 = System.nanoTime()
  val test_userbase = new Userbase_MM()
  var t2 = System.nanoTime()
  elapsedTime = (t2 - t1)/ms
  totalTime += elapsedTime
  println(s"userbase_init(): $elapsedTime ms")


  // get_user test
  t1 = System.nanoTime()
  for (y <- 1 to 943) {
    val user = test_userbase.get_user(y)
    //if (user.userID != -1) println(s"$y: ${user}")
  }
  t2 = System.nanoTime()
  elapsedTime = (t2 - t1)/ms
  totalTime += elapsedTime
  println(s"get_user() for all users: $elapsedTime ms")


  // average_rating test
  t1 = System.nanoTime()
  for (x <- 1 to 1682) {
    val rating = test_userbase.compute_average_rating(x)
    //if (rating != -1) println(s"Average rating for movie $x: ${rating}")
  }
  t2 = System.nanoTime()
  elapsedTime = (t2 - t1)/ms
  totalTime += elapsedTime
  println(s"compute_average_rating() for all movies: $elapsedTime ms")


  // query stress test
  t1 = System.nanoTime()
  for (user_num <- 1 to 943) {
    for (movie_num <- 1 to 1682) {
      val rating = test_userbase.rating_query(user_num, movie_num)
      /*if (rating != -1) {
        println(s"User #$user_num's rating for movie #$movie_num is ${test_userbase.rating_query(user_num, movie_num)} ")
      }*/
    }
  }
  t2 = System.nanoTime()
  elapsedTime = (t2 - t1)/ms
  totalTime += elapsedTime
  println(s"rating_query() for every user and every movie: $elapsedTime ms")

  println()

  println(s"TOTAL TIME ELAPSED: $totalTime ms")


}*/


