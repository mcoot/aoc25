package aoc25.common

extension [K, V](m: Map[K, V])
  def putIfAbsent(k: K, v: V): Map[K, V] = if m.contains(k) then m else m.updated(k, v)