uniform.project("scrooge-generic-shapeless", "commbank.util.scrooge")


libraryDependencies ++= Seq(
  "org.apache.thrift" % "libthrift" % "0.8.0",
  "com.twitter" %% "scrooge-core" % "4.6.0",
  "com.twitter" %% "finagle-thrift" % "6.34.0",
  "com.chuusai" %% "shapeless" % "2.3.1",
  "org.specs2" %% "specs2-core" % "3.8.3" % "test",
  "org.scala-lang" % "scala-reflect" % scalaVersion.value
)

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)


//The two settings below are because scrooge output conflicts with uniform

(scroogeThriftOutputFolder in Compile) <<= (sourceManaged in Compile) (_ / "scrooge")

(scroogeThriftOutputFolder in Test) <<= (sourceManaged in Test) (_ / "scrooge")
