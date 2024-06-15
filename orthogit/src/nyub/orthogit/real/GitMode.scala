package nyub.orthogit.real

enum GitMode(val repr: String):
    case Directory extends GitMode("40000")
    case File extends GitMode("100644")
