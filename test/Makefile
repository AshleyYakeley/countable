default: test

%.out: %
	./$< > $@

%.diff: %.ref %.out
	diff -U0 $^

Count: Count.hs
	ghc $^ -o $@ -package countable

test: Count.diff
	@echo "Success!"

clean:
	rm -f *.o *.hi *.out Count

# switch off intermediate file deletion
.SECONDARY:

.PHONY: *.diff
