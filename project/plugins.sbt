resolvers += "twitter-repo" at "https://maven.twttr.com"

resolvers += Resolver.url("commbank-releases-ivy", new URL("http://commbank.artifactoryonline.com/commbank/ext-releases-local-ivy"))(Patterns("[organization]/[module]_[scalaVersion]_[sbtVersion]/[revision]/[artifact](-[classifier])-[revision].[ext]"))

addSbtPlugin("au.com.cba.omnia" % "uniform-core" % "1.10.0-20160509042812-b923980")

addSbtPlugin("com.twitter" % "scrooge-sbt-plugin" % "4.5.0")
