package config

import server.login.UsernamePasswordCredentials

case class HttpConfig(host: String, port: Option[Int])

case class Config(http: HttpConfig, users: List[UsernamePasswordCredentials])