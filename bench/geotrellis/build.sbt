name := """geotrellis-benchmarks"""

version := "1.0.0"

scalaVersion in ThisBuild := "2.11.12"

/* Settings common to each sub project */
val common = Seq(
  scalacOptions ++= Seq(
    "-deprecation",
    "-language:higherKinds",
    "-Ypartial-unification",
    "-Ywarn-value-discard",
    "-Ywarn-unused-import",
    "-Ywarn-dead-code",
    "-Ywarn-numeric-widen",
  ),

  resolvers ++= Seq(
    "locationtech-releases" at "https://repo.locationtech.org/content/groups/releases",
    "locationtech-snapshots" at "https://repo.locationtech.org/content/groups/snapshots"
  ),

  libraryDependencies ++= Seq(
    "org.locationtech.geotrellis" %% "geotrellis-raster" % "2.0.0-M2"
  )
)

/*
 * Benchmarks can be executed by first switching to the `bench` project and then by running:
    jmh:run -t 1 -f 1 -wi 5 -i 5 .*Bench.*
 */
lazy val lib = project.in(file(".")).enablePlugins(JmhPlugin).settings(common)
