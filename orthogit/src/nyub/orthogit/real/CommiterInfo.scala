package nyub.orthogit.real

import java.time.Instant
import java.time.ZoneId

case class CommitterInfo(
    val name: String,
    val mail: String,
    val timestamp: Long,
    val timezone: String
):
    override def toString(): String =
        val date = Instant.ofEpochSecond(timestamp)
        val zoneId = ZoneId.of(timezone)
        s"${name} ${mail} ${date.atZone(zoneId)}"
