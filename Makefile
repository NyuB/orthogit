ifeq ($(OS), Windows_NT)
# Project paths
	MILLW=millw
# Shell commands
	CP=copy
	RM=del
else
# Project paths
	MILLW=./millw
# Shell commands
	CP=cp
	RM=rm
endif

dev: fmt test

test:
	$(MILLW) assert_extensions.test + orthogit.test

fmt:
	scalafmt .
fmt-check:
	scalafmt --check .

clean:
	$(MILLW) clean
