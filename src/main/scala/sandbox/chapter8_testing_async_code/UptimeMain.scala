package sandbox.chapter8_testing_async_code

import cats.Id

object UptimeMain {
  trait TestUptimeClient extends UptimeClient[Id] {
    def getUptime(hostname: String): Int
  }

  class TestUptimeClientImpl(hosts: Map[String, Int]) extends TestUptimeClient {
    override def getUptime(hostname: String): Int =
      hosts.getOrElse(hostname, 0)
  }

  def testTotalUptime(): Unit = {
    val hosts = Map("host1" -> 10, "host2" -> 6)
    val client = new TestUptimeClientImpl(hosts)
    val service = new UptimeService(client)
    val actual = service.getTotalUptime(hosts.keys.toList)
    val expected = hosts.values.sum
    assert(actual == expected)
  }

  def main(args: Array[String]): Unit = {
    testTotalUptime()
  }
}
