class NetflixMain {

  val mdb = new MovieBaseCDC
  val udb = new Userbase_MM

  val mRatings: Array[Array[Int]] = udb.get_all_ratings()

  for (movie <- mRatings) {mdb.addRating(movie(0), movie(1))} //Add movie ratings to mdb to get later when discerning genres.

}
