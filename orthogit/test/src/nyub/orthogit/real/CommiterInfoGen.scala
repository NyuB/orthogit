package nyub.orthogit.real

import org.scalacheck.Gen

def committerInfoGen: Gen[CommitterInfo] = mailGen.flatMap: mail =>
    nameGen.flatMap: name =>
        epochSecondGen.map: t =>
            CommitterInfo(name, mail, t, "+0200")

private def nameGen = Gen.nonEmptyStringOf(
  Gen.frequency(5 -> Gen.alphaChar, 1 -> Gen.oneOf(' ', '\t', '-', '_'))
)

private def mailGen = Gen
    .nonEmptyStringOf(Gen.alphaNumChar)
    .flatMap: name =>
        Gen.nonEmptyStringOf(Gen.alphaNumChar)
            .flatMap: host =>
                Gen.nonEmptyStringOf(Gen.alphaNumChar)
                    .map: domain =>
                        s"${name}@${host}.${domain}"

private def epochSecondGen = Gen.calendar.map: t =>
    t.toInstant().toEpochMilli() / 1000L
