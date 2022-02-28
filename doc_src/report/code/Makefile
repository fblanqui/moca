FILES = group_list non_commutative_list
MLM_FILES = $(FILES:%=%.mlm)
ML_GENFILES = $(FILES:%=%.ml)
MOCA = mocac

default: $(ML_GENFILES)

.SUFFIXES: .ml .mlm

.mlm.ml:
	$(MOCA) $<

clean:
	rm -f $(ML_GENFILES)