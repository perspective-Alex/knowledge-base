MYSTEM_DIR = ./resources/Mystem
TOMITA_DIR = ./resources/Tomita
RESULTS_DIR = ./results
CONTENT_FILE2 = ./data/test/ont3.txt
CONTENT_FILE3 = ./data/test/ont4.txt
MORPH_FILE2 = ./data/test/mf_ont3.txt
MORPH_FILE3 = ./data/test/mf_ont4.txt
OUT_FILE = ./output.txt

CONTENTS = $(CONTENT_FILE2) $(CONTENT_FILE3)
MORPHS = $(MORPH_FILE2) $(MORPH_FILE3)

mf:
	$(MYSTEM_DIR)/mystem -n -i -d $(CONTENT_FILE2) $(MORPH_FILE2)
	$(MYSTEM_DIR)/mystem -n -i -d $(CONTENT_FILE3) $(MORPH_FILE3)
build:
	stack build
exec:
	stack exec knBase $(OUT_FILE) $(CONTENTS) $(MORPHS) >> \
	$(RESULTS_DIR)/out/ont/res.txt
	cd $(TOMITA_DIR) ; \
	sed -i 's/\"//g' ./keywords.txt ; \
	./tomita-parser config_def.proto ; \
  ./tomita-parser config_rel.proto ;
	cp $(TOMITA_DIR)/*.html $(RESULTS_DIR)/out/ont

exec_simple:
	stack exec knBase $(OUT_FILE) $(CONTENTS) $(MORPHS)
	cd $(TOMITA_DIR) ; \
	sed -i 's/\"//g' ./keywords.txt ; \
	./tomita-parser config_def.proto ; \
  ./tomita-parser config_rel.proto ;

pre:
	sed -i ':a; /$$/N; s/\- \{0,3\}\n//; ta' $(CONTENTS)
	sed -i ':a; /$$/N; s/\n/ /; ta' $(CONTENTS)
	cat $(CONTENTS) >> $(TOMITA_DIR)/input1.txt

clean: 
	echo "" > $(TOMITA_DIR)/input1.txt

all: clean pre mf build exec 

simple: clean pre mf build exec_simple
