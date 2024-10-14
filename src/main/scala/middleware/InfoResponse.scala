package middleware

import agents.GameAgent

object GameInfo {
  trait InfoResponse {
    val agent: GameAgent
  }
}