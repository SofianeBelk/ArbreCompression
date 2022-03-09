CC := ocamlbuild
RM ?= rm -f

EXEC := bst_compress
TEST := test

SRCDIR := src
TESTDIR := test
BUILDDIR := _build

SRC := $(wildcard $(SRCDIR)/*.ml)
TESTS := $(wildcard $(TESTDIR)/*.ml)

CFLAGS :=
LDFLAGS :=

.PHONY: native
native:
	$(CC) $(CFLAGS) $(SRCDIR)/$(EXEC).$@

.PHONY: byte
byte:
	$(CC) $(CFLAGS) $(SRCDIR)/$(EXEC).$@

.PHONY: all
all: native byte test

.PHONY: test
test:
	$(CC) $(CFLAGS) $(TESTDIR)/$(TEST).native -I $(SRCDIR)

.PHONY: clean
clean:
	$(RM) -r $(BUILDDIR)

.PHONY: cleanall
cleanall:
	$(RM) -r $(BUILDDIR) $(EXEC).* $(TEST).*